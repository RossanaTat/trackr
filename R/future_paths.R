#' Prepare data for future target projections
#'
#'
#' @inheritParams prep_data
#' @return A `data.table` with the latest observation per country, including:
#' \describe{
#'   \item{code}{Standardized country code (from `code_col`)}
#'   \item{year}{Year of the latest available observation}
#'   \item{y}{Value of the indicator}
#'   \item{y_fut}{Rounded value of `y`, based on the specified granularity}
#' }
#' @export
prep_data_fut <- function(data               = NULL,
                          indicator          = "EG.ELC.ACCS.ZS",
                          granularity        = 0.1,
                          code_col           = "iso3c",
                          year_col           = "date",
                          support            = 1,
                          extreme_percentile = getOption("trackr.extreme_pctl"),
                          startyear_data     = 2000,
                          endyear_data       = 2023,
                          verbose            = TRUE) {

  # Convert to data.table
  dt <- qDT(data)

  # Standardize column names
  data.table::setnames(dt,
                       old         = c(code_col, year_col, indicator),
                       new         = c("code", "year", "y"),
                       skip_absent = FALSE)

  # Keep relevant columns and latest observation per country
  # Filter to intended future horizon

  dt <- dt[!is.na(y), .(code, year, y)]
  dt <- dt[year >= startyear_data & year <= endyear_data]

  setorder(dt, code, year)

  dt <- dt[, .SD[.N], by = code]

  if (nrow(dt) == 0) {
    cli::cli_warn("No data remains within startyear_data and endyear_data window.")
    return(NULL)
  }


  # ____________________________
  # Tail-aware support filtering on raw y
  # ____________________________

  # Compute percentiles of y
  # --------------------------------------
  # Tail cutoff logic: support asymmetric percentiles ####
  # --------------------------------------
  if (length(extreme_percentile) == 1) {
    lower_pct <- extreme_percentile
    upper_pct <- extreme_percentile
  } else if (length(extreme_percentile) == 2) {
    lower_pct <- extreme_percentile[1]
    upper_pct <- extreme_percentile[2]
  } else {
    cli::cli_abort("extreme_percentile must be a numeric vector of length 1 or 2.")
  }

  # Compute percentile-based cutoffs
  lower_cutoff <- quantile(dt$y, probs = lower_pct, na.rm = TRUE)
  upper_cutoff <- quantile(dt$y, probs = 1 - upper_pct, na.rm = TRUE)



  # Compute bin on y for support checking
  dt[, y_bin := round(y / granularity) * granularity]

  # Support counts by raw y bin
  support_table <- dt[, .(n_countries = uniqueN(code)), by = y_bin]
  support_table[, region := fifelse(y_bin < lower_cutoff, "low",
                                    fifelse(y_bin > upper_cutoff, "high", "middle"))]
  support_table[, keep := region == "middle" | n_countries >= support]

  supported_bins <- support_table[keep == TRUE, y_bin]

  # Filter based on support in y bins
  dt <- dt[y_bin %in% supported_bins]

  # Now compute y_fut from filtered y
  dt[, y_fut := round(y / granularity) * granularity]
  dt[, y_bin := NULL]  # clean up

  if (verbose && support >= 1) {
    cli::cli_alert_info("{length(supported_bins)} starting levels retained after applying support >= {support} to tails.")
  }

  return(dt)
}


# -------------------- #
# Percentiles ~~ #
# -------------------- #

#' Generate Future Percentile Paths
#'
#' Computes projected future paths for given percentiles based on initial levels and predicted changes.
#'
#' @param data_fut A data frame containing historical data with columns `code`, `year`, and `y`. This serves as the baseline for projections.
#' @param target_year An integer specifying the year to project to. Default is '2030'. Only relevant if `future = TRUE`.
#' @inheritParams path_historical
#' @param verbose Logical; if `TRUE`, messages will be printed for each processing year. Default is `TRUE`.
#'
#' @return A data.table with the projected values (`y_fut`) by `code`, `year`, and `pctl`, including observed values where available.
#'
#' @examples
#' \dontrun{
#' data_fut <- data.frame(code = c("A", "A"), year = c(2020, 2021), y = c(50, 52))
#' predictions_pctl <- data.frame(initialvalue = 52, pctl = 20, change = 1)
#' future_path_pctls(data_fut = data_fut, predictions_pctl = predictions_pctl, target_year = 2025)
#' }
#'
#' @export
future_path_pctls <- function(data_fut,
                              sequence_pctl     = seq(20,80,20),
                              changes_pctl,
                              target_year = 2030,
                              granularity = 0.1,
                              min         = NULL,
                              max         = NULL,
                              verbose     = TRUE) {

  # Create a new dataset which will contain the predicted path from the last observation to targetyear at all selected percentiles

  path_fut_pctl <- expand.grid(code = unique(data_fut$code),
                               year = seq(min(data_fut$year),
                                        target_year,
                                        1),
                               pctl = sequence_pctl) |>
    # Merge in the actual data from WDI
    joyn::joyn(data_fut,
               by = c("code","year"),
               match_type ="m:1",
               keep = "left",
               reportvar = FALSE,
               verbose = FALSE)

  # Year-by-year, calculate the percentile paths for the last value observed. For future target creation.

  cli::cli_alert_info("Calculating future percentile paths")

  startyear_target = min(path_fut_pctl$year)

  n = startyear_target - min(path_fut_pctl$year) + 1

  # Continue this iteratively until the target year
  while (n + min(path_fut_pctl$year) - 1 <= target_year) {
    # Year processed concurrently
    path_fut_pctl <- path_fut_pctl |>
      # Merge in data with predicted changes based on initial levels
      joyn::joyn(changes_pctl,
                 match_type = "m:1",
                 keep = "left",
                 by = c("y_fut = y",
                        "pctl"),
                 reportvar = FALSE,
                 verbose = FALSE) |>
      group_by(code,
               pctl) |>
      arrange(year) |>
      # Calculate new level based on the predicted changes.
      mutate(y_pctl = if_else(row_number()==n & !is.na(lag(y_fut)),
                             round((lag(y_fut)+lag(change))/granularity)*granularity, y_fut)) |>
      ungroup() |>
      select(-change)

    n=n+1
  }

  path_fut_pctl <- qDT(path_fut_pctl)[
    !is.na(y_pctl) & (is.na(y) | (y >= min & y <= max))
  ][
    , y_fut := NULL
  ][
    , setorder(.SD, code, year)
  ]

  # Return ####
  return(path_fut_pctl)
}

# -------------------- #
# Speed ~~ #
# -------------------- #

#' Project future indicator paths at different speeds, including historical speeds
#'
#' This function simulates future indicator trajectories under different speeds of progress.
#' If "his" is included in `sequence_speed`, it will use each country's historical speed score.
#'
#' @param data_fut A data.table containing the historical/baseline data. Must include `code`, `year`, and `y_fut`.
#' @param sequence_speed Numeric vector of speeds to project. Can include "his" to use historical speed.
#' @param path_speed A typical path of progress (from worst to best outcome).
#' @param scores_his Optional: historical speed scores table from `get_scores_speed()` (required if "his" in sequence_speed)
#' @param best "high" or "low" (indicator direction)
#' @param target_year Year to project to
#' @param min Minimum allowed value
#' @param max Maximum allowed value
#'
#' @return data.table with projected `y_fut` for each country-year-speed combination
#' @export
future_path_speed <- function(data_fut,
                              sequence_speed    = c(0.25,0.5,1,2,4,"his"),
                              path_speed,
                              best        = "high",
                              target_year = 2030,
                              min         = NULL,
                              max         = NULL,
                              eval_from   = NULL,
                              eval_to     = NULL,
                              path_his_speed = NULL) {

  dt <- qDT(data_fut)

  # -------------------------------
  # 1. Compute historical speeds if "his" is requested
  # -------------------------------
  if ("his" %in% sequence_speed) {
    if (is.null(path_his_speed)) {
      cli::cli_abort("`path_his_speed` must be provided when 'his' is included in sequence_speed.")
    }

    his_speed_dt <- path_his_speed[, .(code, his_speed = score)]

    missing_speed <- his_speed_dt[is.na(his_speed), code]
    if (length(missing_speed) > 0) {
      cli::cli_warn("Historical speed missing for country(ies): {paste(missing_speed, collapse=', ')}. Dropping from projections.")
      his_speed_dt <- his_speed_dt[!is.na(his_speed)]
    }

    # Merge historical speeds into data
    dt <- dt[his_speed_dt, on = "code"]
  }

  # -------------------------------
  # 2. Expand data by sequence_speed
  # -------------------------------
  expanded <- CJ(code = unique(dt$code),
                 year = seq(min(dt$year), target_year, 1),
                 speed = sequence_speed,
                 unique = TRUE)

  dt <- expanded[dt, on = .(code), allow.cartesian = TRUE]

  # -------------------------------
  # 3. Convert speed column to numeric
  # -------------------------------
  dt[, speed_num := fifelse(speed == "his", his_speed, as.numeric(speed))]

  # Drop countries without numeric speed
  dt <- dt[!is.na(speed_num)]

  # -------------------------------
  # 4. Cross join with path_speed lookup
  # -------------------------------
  dt <- dt[path_speed, on = "y", allow.cartesian = TRUE]
  dt[, best := best]

  # Apply best direction filter
  if (best == "high") dt <- dt[y <= y_speed]
  if (best == "low")  dt <- dt[y >= y_speed]

  # -------------------------------
  # 5. Compute projected years and values
  # -------------------------------
  dt <- dt[order(code, speed_num, time)]
  dt[, year := round(year + (time - time[1]) / speed_num), by = .(code, speed)]

  dt[, y_fut := zoo::na.approx(y_speed, year, na.rm = FALSE, rule = 2), by = .(code, speed)]

  # -------------------------------
  # 6. Keep only relevant columns and within bounds
  # -------------------------------
  dt <- dt[is.na(y_fut) | (y_fut >= min & y_fut <= max)]
  dt <- dt[, .(code, year, speed, y_fut)]
  setorder(dt, code, year, speed)

  return(dt)
}


#' Future paths. Wrapper to Compute future paths based on either percentiles or speeds method
#'
#' Computes future trajectories for each country either based on percentile projections (`future_path_pctls()`) or speed of progress (`future_path_speed()`), depending on user input.
#'
#'
#' @inheritParams future_path_pctls
#' @inheritParams future_path_speed
#' @param speed Logical. If `TRUE`, calls `future_path_speed()`.
#' @param percentiles Logical. If `TRUE`, calls `future_path_pctls()`.
#'
#' @return A `data.table` with projected paths based on selected method.
#'
#' @seealso [future_path_pctls()], [future_path_speed()]
#' @export
# future_path <- function(data_fut,
#                         target_year      = 2030,
#                         min              = 0,
#                         max              = 100,
#                         granularity      = 0.1,
#                         sequence_pctl    = seq(20, 80, 20),
#                         changes_pctl     = NULL,
#                         sequence_speed   = c(0.25, 0.5, 1, 2, 4),
#                         path_speed       = NULL,
#                         best             = "high",
#                         verbose          = TRUE,
#                         speed            = FALSE,
#                         percentiles      = FALSE) {
#
#   result <- list(pctl  = NULL,
#                  speed = NULL)
#
#   if (percentiles) {
#     if (is.null(changes_pctl)) {
#       cli::cli_abort("`changes_pctl` must be provided when `percentiles = TRUE`.")
#     }
#
#     result$pctl <- future_path_pctls(
#       data_fut         = data_fut,
#       sequence_pctl    = sequence_pctl,
#       changes_pctl     = changes_pctl,
#       target_year      = target_year,
#       granularity      = granularity,
#       min              = min,
#       max              = max,
#       verbose          = verbose
#     )
#   }
#
#   if (speed) {
#     if (is.null(path_speed)) {
#       cli::cli_abort("`path_speed` must be provided when `speed = TRUE`.")
#     }
#
#     result$speed <- future_path_speed(
#       data_fut       = data_fut,
#       sequence_speed = sequence_speed,
#       path_speed     = path_speed,
#       best           = best,
#       target_year    = target_year,
#       min            = min,
#       max            = max
#     )
#   }
#
#   if (is.null(result$pctl) && is.null(result$speed)) {
#     cli::cli_abort("At least one of `percentiles = TRUE` or `speed = TRUE` must be specified.")
#   }
#
#   return(result)
# }
#
# -------------------- #
# Future paths. Wrapper with "his" support
# -------------------- #
future_path <- function(data_fut,
                        target_year      = 2030,
                        min              = 0,
                        max              = 100,
                        granularity      = 0.1,
                        sequence_pctl    = seq(20, 80, 20),
                        changes_pctl     = NULL,
                        sequence_speed   = c(0.25, 0.5, 1, 2, 4),
                        path_speed       = NULL,
                        scores_his       = NULL, # new argument for historical speed
                        best             = "high",
                        verbose          = TRUE,
                        speed            = FALSE,
                        percentiles      = FALSE) {

  result <- list(pctl  = NULL,
                 speed = NULL)

  # ------------------- #
  # Percentiles
  # ------------------- #
  if (percentiles) {
    if (is.null(changes_pctl)) {
      cli::cli_abort("`changes_pctl` must be provided when `percentiles = TRUE`.")
    }

    result$pctl <- future_path_pctls(
      data_fut         = data_fut,
      sequence_pctl    = sequence_pctl,
      changes_pctl     = changes_pctl,
      target_year      = target_year,
      granularity      = granularity,
      min              = min,
      max              = max,
      verbose          = verbose
    )
  }

  # ------------------- #
  # Speed
  # ------------------- #
  if (speed) {

    if (is.null(path_speed)) {
      cli::cli_abort("`path_speed` must be provided when `speed = TRUE`.")
    }

    # --- Handle "his" in sequence_speed
    use_his <- "his" %in% sequence_speed
    numeric_speeds <- sequence_speed[sequence_speed != "his"]

    speed_input <- data.table(code = unique(data_fut$code))

    if (use_his) {
      if (is.null(scores_his) || !"score" %in% names(scores_his)) {
        cli::cli_abort("Historical speed scores not provided or missing `score` column.")
      }

      # Keep only countries with historical speed
      his_speeds <- scores_his[!is.na(score), .(code, speed = score)]

      missing_countries <- setdiff(unique(scores_his$code), his_speeds$code)
      if (length(missing_countries) > 0) {
        cli::cli_warn("Dropping {length(missing_countries)} countries with missing historical speed: {paste(missing_countries, collapse=', ')}")
      }

      # Merge historical speeds into the data
      speed_input <- merge(speed_input, his_speeds, by = "code", all.x = FALSE)
    }

    # Now call future_path_speed
    result$speed <- future_path_speed(
      data_fut        = data_fut[speed_input, on = "code"], # filter countries with historical speeds
      sequence_speed  = numeric_speeds,
      path_speed      = path_speed,
      best            = best,
      target_year     = target_year,
      min             = min,
      max             = max,
      scores_his      = if (use_his) his_speeds else NULL # pass historical speed scores
    )
  }

  if (is.null(result$pctl) && is.null(result$speed)) {
    cli::cli_abort("At least one of `percentiles = TRUE` or `speed = TRUE` must be specified.")
  }

  return(result)
}
