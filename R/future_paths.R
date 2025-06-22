#' Prepare data for future target projections
#'
#'
#' @param data A `data.frame` or `data.table` containing the indicator data. Defaults to the result of
#' `wbstats::wb_data()` for the specified indicator. The dataset must include columns for the country code,
#' year, and indicator value.
#' @param indicator Character. The name of the World Bank Development Indicator to use. Default is `"EG.ELC.ACCS.ZS"`,
#' which measures the percentage of the population with access to electricity.
#' @param granularity Numeric. The level of granularity for the output variable `y_fut`. For example, `0.1` rounds to
#' one decimal place (e.g., 43.6%), while `1` rounds to the nearest whole number (e.g., 44%). Lower values yield
#' more precise projections but may increase computing time. Default is `0.1`.
#' @param code_col Character. Name of the column containing the country code (e.g., ISO3 code). Default is `"iso3c"`.
#' @param year_col Character. Name of the column containing the year. Default is `"date"`.
#' @param verbose Logical. If `TRUE`, the function may print messages. Default is `TRUE`.
#'
#' @return A `data.table` with the latest observation per country, including:
#' \describe{
#'   \item{code}{Standardized country code (from `code_col`)}
#'   \item{year}{Year of the latest available observation}
#'   \item{y}{Value of the indicator}
#'   \item{y_fut}{Rounded value of `y`, based on the specified granularity}
#' }
#' @export
prep_data_fut <- function(data           = wbstats::wb_data(indicator = indicator, lang = "en", country = "countries_only"),
                          indicator      = "EG.ELC.ACCS.ZS",
                          granularity    = 0.1,
                          code_col       = "iso3c",
                          year_col       = "date",
                          verbose = TRUE) {

  # Convert to data.table
  dt <- data.table::as.data.table(data)

  # Standardize column names
  data.table::setnames(dt,
                       old         = c(code_col, year_col, indicator),
                       new         = c("code", "year", "y"),
                       skip_absent = FALSE)

  # Keep only relevant columns
  dt <- dt[, .(code, year, y)]

  # Remove NAs and keep only the last observation for each code
  dt <- dt[!is.na(y)]
  data.table::setorder(dt, code, year)
  dt <- dt[, .SD[.N], by = code]

  # Compute future value based on rounded y
  dt[, y_fut := round(y / granularity) * granularity]

  return(dt)
}

# -------------------- #
# Percentiles ~~ #
# -------------------- #

#' Generate Future Percentile Paths
#'
#' Computes projected future paths for given percentiles based on initial levels and predicted changes.
#'
#' This function builds a dataset that extends observed data (`data_fut`) forward in time to a target year using
#' predicted changes at specified percentiles. It iteratively calculates future values by applying changes year-by-year,
#' starting from the last available observation for each country-percentile pair.
#'
#' @param data_fut A data frame containing historical data with columns `code`, `year`, and `y`. This serves as the baseline for projections.
#' @param target_year An integer specifying the year to project to. Default is `2030`.
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
                              min         = 0,
                              max         = 100,
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
    #print(n+min(path_fut_pctl$year)-1)

    if (verbose) cli::cli_alert_info("Processing year {.strong {(n + min(path_fut_pctl$year) - 1)}}")

    path_fut_pctl <- path_fut_pctl |>
      # Merge in data with predicted changes based on initial levels
      joyn::joyn(changes_pctl,
                 match_type = "m:1",
                 keep = "left",
                 by = c("y_fut=initialvalue","pctl"),
                 reportvar = FALSE,
                 verbose = FALSE) |>
      group_by(code,
               pctl) |>
      arrange(year) |>
      # Calculate new level based on the predicted changes.
      mutate(y_fut = if_else(row_number()==n & !is.na(lag(y_fut)),
                             round((lag(y_fut)+lag(change))/granularity)*granularity,y_fut)) |>
      ungroup() |>
      select(-change)

    n=n+1
  }

  path_fut_pctl <- as.data.table(path_fut_pctl)[!is.na(y_fut) & (is.na(y) | (y >= min & y <= max))] |>
    setorder(code,
             year)


  # Return ####
  return(path_fut_pctl)
}

# -------------------- #
# Speed ~~ #
# -------------------- #

#' Project Future Indicator Pathways at Different Speeds
#'
#' This function simulates future indicator trajectories under different
#' speeds of progress, based on current trends and a set of predefined
#' projection speeds. It generates interpolated yearly projections from
#' the latest available data point up to a given target year, filtering
#' for progress scenarios that either increase or decrease the indicator
#' as specified.
#'
#' @param data_fut A data frame or data table containing the historical and/or
#'   baseline data. Must include columns: `code`, `year`, and `y` (the indicator),
#'   and optionally `y_fut` and `time` for projections.
#' @inheritParams future_path_pctls
#' @inheritParams get_speed_path
#'
#' @return A data table with projected indicator values (`y_fut`) for each
#'   country-year-speed combination from the last available year through
#'   the `target_year`, respecting the selected progress speeds and
#'   indicator direction (`best`).
#' @export
future_path_speed <- function(data_fut,
                              speedseq    = c(0.25,0.5,1,2,4),
                              path_speed,
                              best        = "high",
                              target_year = 2030,
                              min         = 0,
                              max         = 100) {


  # Creates dataset with the country-years-speeds where projections are needed
  data_fut <- expand.grid(code = unique(data_fut$code),
                          year = seq( min (data_fut$year),
                                   target_year,
                                   1),
                          speed = speedseq) |>
    rename("yeartemp"          = "year") |>
    joyn::joyn(data_fut,
               match_type      = "m:1",
               by              = "code",
               reportvar       = FALSE,
               verbose         = FALSE,
               y_vars_to_keep  = "year") |>

    filter(yeartemp >= year) |>
    select(-year) |>
    rename("year"              = "yeartemp") |>

    joyn::joyn(data_fut,
               match_type      = "m:1",
               by              = c("code",
                    "year"),
               reportvar       = FALSE,
               verbose = FALSE)

  # Create a new dataset which will eventually contain the predicted path from the last observation to targetyear at all selected speeds
  path_fut_speed <- data_fut |>
    select(-y) |>
    filter(!is.na(y_fut)) |>
    cross_join(path_speed) |>
    mutate(best = best) |>
    filter(if_else(best == "high",
                   y_fut <= y,
                   y_fut >= y)) |>
    group_by(code,
             speed) |>
    arrange(time) |>
    mutate(year = year + (time-time[1])/speed) |>
    ungroup() |>
    select(-c(y_fut,
              time,
              best)) |>
    rename("y_fut" = "y") |>

    joyn::joyn(data_fut,
               match_type = "1:1",
               by = c("code","year","speed"),
               reportvar = FALSE,
               verbose = FALSE,
               y_vars_to_keep = "y") |>
    group_by(code,
             speed) |>
    arrange(year) |>
    mutate(y_fut = zoo::na.approx(y_fut,
                                  year,
                                  na.rm = FALSE,
                                  rule=2)) |>
    filter(year %in% seq(min(year),
                         target_year,
                         1)) |>
    ungroup() |>
    filter(!is.na(y_fut))

    # Only keep cases where target has not been reached
    #filter(between(y,min,max) | is.na(y))

  path_fut_speed <- as.data.table(path_fut_speed)[is.na(y) | (y >= min & y <= max)] |>
    setorder(code, year)


  return(path_fut_speed)
}

#' Wrapper to Compute Future Paths Based on Either Percentiles or Speeds method
#'
#' Computes future trajectories for each country either based on percentile growth projections
#' (`future_path_pctls()`) or speed of progress (`future_path_speed()`), depending on user input.
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
future_path <- function(data_fut,
                        target_year      = 2030,
                        min              = 0,
                        max              = 100,
                        granularity      = 0.1,
                        sequence_pctl    = seq(20, 80, 20),
                        changes_pctl     = NULL,
                        speedseq         = c(0.25, 0.5, 1, 2, 4),
                        path_speed       = NULL,
                        best             = "high",
                        verbose          = TRUE,
                        speed            = FALSE,
                        percentiles      = FALSE) {

  result <- list(pctl = NULL, speed = NULL)

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

  if (speed) {
    if (is.null(path_speed)) {
      cli::cli_abort("`path_speed` must be provided when `speed = TRUE`.")
    }

    result$speed <- future_path_speed(
      data_fut     = data_fut,
      speedseq     = speedseq,
      path_speed   = path_speed,
      best         = best,
      target_year  = target_year,
      min          = min,
      max          = max
    )
  }

  if (is.null(result$pctl) && is.null(result$speed)) {
    cli::cli_abort("At least one of `percentiles = TRUE` or `speed = TRUE` must be specified.")
  }

  return(result)
}


## TESTING NEW FUNCTIONS ####
new_future_path_pctls <- function(data_fut,
                                  sequence_pctl     = seq(20, 80, 20),
                                  changes_pctl,
                                  target_year = 2030,
                                  granularity = 0.1,
                                  min         = 0,
                                  max         = 100,
                                  verbose     = TRUE) {


  # Step 1: Start from observed data_fut at the base year
  dt_start <- data_fut[, .(code, year, y, y_fut)]
  dt_start <- dt_start[, .(pctl = sequence_pctl), by = .(code, year, y, y_fut)]

  # Step 2: Expand to all years forward
  all_years <- seq(min(dt_start$year),
                   target_year)

  path_fut_pctl <- dt_start[, .(year = all_years),
                            by = .(code, pctl, y, y_fut)]

  # Step 3: Keep y_fut only for start year
  base_year <- min(all_years)

  path_fut_pctl[, y_fut := fifelse(year == base_year,
                                   y_fut,
                                   NA_real_)]

  # Step 4: Pre-sort for fast group operations
  setorder(path_fut_pctl, code, pctl, year)

  years_to_project <- all_years[all_years > base_year]

  if (verbose) cli::cli_alert_info("Calculating future percentile paths")

  # Step 5: Year-by-year projection loop
  for (yr in years_to_project) {
    if (verbose) cli::cli_alert_info("Processing year {.strong {yr}}")

    # Merge predictions based on last year’s value
    path_fut_pctl <- joyn::joyn(
      path_fut_pctl,
      changes_pctl,
      by = c("y_fut = initialvalue", "pctl"),
      match_type = "m:1",
      keep = "left",
      reportvar = FALSE,
      verbose = FALSE
    )

    # Apply projected value only for this year
    path_fut_pctl[year == yr,
                  y_fut := round((shift(y_fut) + shift(change)) / granularity) * granularity,
                  by = .(code, pctl)
    ]

    # Drop change to avoid accumulation
    path_fut_pctl[, change := NULL]
  }

  # Step 6: Final cleanup and filtering
  path_fut_pctl <- path_fut_pctl[
    !is.na(y_fut) & (is.na(y) | (y >= min & y <= max))
  ]
  data.table::setorder(path_fut_pctl, code, year)

  # Return result
  return(path_fut_pctl[])
}
