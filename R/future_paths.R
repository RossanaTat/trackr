#' Prepare data for future target projections
#'
#' @inheritParams prep_data
#' @return A `data.table` with the latest observation per country, including:
#'   \item{code}{Standardized country code (from `code_col`)}
#'   \item{year}{Year of the latest available observation}
#'   \item{y}{Value of the indicator}
#'   \item{y_fut}{Rounded value of `y`, based on the specified granularity}
#' @export
prep_data_fut <- function(data = NULL,
                          indicator = "EG.ELC.ACCS.ZS",
                          granularity = 0.1,
                          code_col = "iso3c",
                          year_col = "date",
                          support = 1,
                          extreme_percentile = getOption("trackr.extreme_pctl"),
                          startyear_data = 2000,
                          endyear_data = 2023,
                          verbose = TRUE) {
  dt <- qDT(data)
  data.table::setnames(dt,
    old = c(code_col, year_col, indicator),
    new = c("code", "year", "y"),
    skip_absent = FALSE
  )
  dt <- dt[!is.na(y), .(code, year, y)]
  dt <- dt[year >= startyear_data & year <= endyear_data]
  setorder(dt, code, year)
  dt <- dt[, .SD[.N], by = code]
  if (nrow(dt) == 0) {
    cli::cli_warn("No data remains within startyear_data and endyear_data window.")
    return(NULL)
  }
  if (length(extreme_percentile) == 1) {
    lower_pct <- extreme_percentile
    upper_pct <- extreme_percentile
  } else if (length(extreme_percentile) == 2) {
    lower_pct <- extreme_percentile[1]
    upper_pct <- extreme_percentile[2]
  } else {
    cli::cli_abort("extreme_percentile must be a numeric vector of length 1 or 2.")
  }
  lower_cutoff <- quantile(dt$y, probs = lower_pct, na.rm = TRUE)
  upper_cutoff <- quantile(dt$y, probs = 1 - upper_pct, na.rm = TRUE)
  dt[, y_bin := round(y / granularity) * granularity]
  support_table <- dt[, .(n_countries = uniqueN(code)), by = y_bin]
  support_table[, region := fifelse(y_bin < lower_cutoff, "low",
    fifelse(y_bin > upper_cutoff, "high", "middle"))]
  support_table[, keep := region == "middle" | n_countries >= support]
  supported_bins <- support_table[keep == TRUE, y_bin]
  dt <- dt[y_bin %in% supported_bins]
  dt[, y_fut := round(y / granularity) * granularity]
  dt[, y_bin := NULL]
  if (verbose && support >= 1) {
    cli::cli_alert_info("{length(supported_bins)} starting levels retained after applying support >= {support} to tails.")
  }
  return(dt)
}

#' Project future indicator paths using a vector of speeds
#'
#' @param data_fut Data.table with columns code, year, y, y_fut
#' @param sequence_speed Numeric vector of speeds (applied to all countries)
#' @param path_speed Data.table with speed paths
#' @param best "high" or "low"
#' @param target_year Integer, last year to project
#' @param min Minimum value for y_fut
#' @param max Maximum value for y_fut
#' @return data.table with projections for each speed
#' @export
future_path_sequence_speed <- function(data_fut,
                                       sequence_speed = c(0.25, 0.5, 1, 2, 4),
                                       path_speed,
                                       best = "high",
                                       target_year = 2030,
                                       min = 0,
                                       max = 100) {
  grid <- data.table::CJ(
    code = unique(data_fut$code),
    speed = sequence_speed,
    unique = TRUE
  )
  grid <- merge(grid, data_fut, by = "code", all.x = TRUE)
  grid <- grid[, .(year = seq(min(year), target_year, 1)), by = .(code, speed, y, y_fut)]
  grid <- joyn::joyn(grid, path_speed, match_type = "m:1", by = c("y_fut = y"), reportvar = FALSE, verbose = FALSE)
  grid[, best := best]
  grid <- grid[(best == "high" & y_fut <= y) | (best == "low" & y_fut >= y)]
  grid[, year := year + (time - time[1]) / speed, by = .(code, speed)]
  grid[, c("best", "y_fut", "time") := NULL]
  setnames(grid, "y", "y_speed")
  grid <- joyn::joyn(grid, data_fut, match_type = "m:m", by = c("code", "year", "speed"), reportvar = FALSE, verbose = FALSE, y_vars_to_keep = "y")
  setorder(grid, code, speed, year)
  grid[, y_speed := zoo::na.approx(y_speed, year, na.rm = FALSE, rule = 2), by = .(code, speed)]
  grid <- grid[year %in% seq(min(year), target_year, 1)]
  grid <- grid[is.na(y_speed) | (y_speed >= min & y_speed <= max)]
  grid[]
}

#' Project future indicator paths using historical speed scores
#'
#' @param data_fut Data.table with columns code, year, y, y_fut
#' @param speed_scores Data.table with columns code, speed (one per country)
#' @param path_speed Data.table with speed paths
#' @param best "high" or "low"
#' @param target_year Integer, last year to project
#' @param min Minimum value for y_fut
#' @param max Maximum value for y_fut
#' @return data.table with projections for each country's historical speed
#' @export
future_path_historical_speed <- function(data_fut,
                                         speed_scores,
                                         path_speed,
                                         best = "high",
                                         target_year = 2030,
                                         min = 0,
                                         max = 100) {
  dt <- merge(data_fut, speed_scores, by = "code", all.x = TRUE)
  if (any(is.na(dt$speed))) stop("Missing speed score for some countries.")
  grid <- dt[, .(year = seq(min(year), target_year, 1)), by = .(code, speed, y, y_fut)]
  grid <- joyn::joyn(grid, path_speed, match_type = "m:1", by = c("y_fut = y"), reportvar = FALSE, verbose = FALSE)
  grid[, best := best]
  grid <- grid[(best == "high" & y_fut <= y) | (best == "low" & y_fut >= y)]
  grid[, year := year + (time - time[1]) / speed, by = .(code, speed)]
  grid[, c("best", "y_fut", "time") := NULL]
  setnames(grid, "y", "y_speed")
  grid <- joyn::joyn(grid, data_fut, match_type = "m:m", by = c("code", "year", "speed"), reportvar = FALSE, verbose = FALSE, y_vars_to_keep = "y")
  setorder(grid, code, speed, year)
  grid[, y_speed := zoo::na.approx(y_speed, year, na.rm = FALSE, rule = 2), by = .(code, speed)]
  grid <- grid[year %in% seq(min(year), target_year, 1)]
  grid <- grid[is.na(y_speed) | (y_speed >= min & y_speed <= max)]
  grid[]
}

#' Generate Future Percentile Paths
#'
#' Computes projected future paths for given percentiles based on initial levels and predicted changes.
#'
#' @param data_fut A data frame containing historical data with columns `code`, `year`, and `y`.
#' @param sequence_pctl Numeric vector of percentiles
#' @param changes_pctl Data.table with predicted changes
#' @param target_year Integer, last year to project
#' @param granularity Numeric, rounding precision
#' @param min Minimum value for y
#' @param max Maximum value for y
#' @param verbose Logical
#' @return A data.table with the projected values (`y_fut`) by `code`, `year`, and `pctl`
#' @export
future_path_pctls <- function(data_fut,
                              sequence_pctl = seq(20, 80, 20),
                              changes_pctl,
                              target_year = 2030,
                              granularity = 0.1,
                              min = NULL,
                              max = NULL,
                              verbose = TRUE) {
  path_fut_pctl <- expand.grid(
    code = unique(data_fut$code),
    year = seq(min(data_fut$year), target_year, 1),
    pctl = sequence_pctl
  ) |>
    joyn::joyn(data_fut,
      by = c("code", "year"),
      match_type = "m:1",
      keep = "left",
      reportvar = FALSE,
      verbose = FALSE
    )
  cli::cli_alert_info("Calculating future percentile paths")
  startyear_target <- min(path_fut_pctl$year)
  n <- startyear_target - min(path_fut_pctl$year) + 1
  while (n + min(path_fut_pctl$year) - 1 <= target_year) {
    path_fut_pctl <- path_fut_pctl |>
      joyn::joyn(changes_pctl,
        match_type = "m:1",
        keep = "left",
        by = c("y_fut = y", "pctl"),
        reportvar = FALSE,
        verbose = FALSE
      ) |>
      dplyr::group_by(code, pctl) |>
      dplyr::arrange(year) |>
      dplyr::mutate(y_pctl = dplyr::if_else(
        dplyr::row_number() == n & !is.na(dplyr::lag(y_fut)),
        round((dplyr::lag(y_fut) + dplyr::lag(change)) / granularity) * granularity,
        y_fut
      )) |>
      dplyr::ungroup() |>
      dplyr::select(-change)
    n <- n + 1
  }
  path_fut_pctl <- qDT(path_fut_pctl)[
    !is.na(y_pctl) & (is.na(y) | (y >= min & y <= max))
  ][, y_fut := NULL][, setorder(.SD, code, year)]
  path_fut_pctl
}

#' Wrapper for future projections
#'
#' @param data_fut Data.table with columns code, year, y, y_fut
#' @param sequence_speed Numeric vector of speeds
#' @param path_speed Data.table with speed paths
#' @param speed_scores Data.table with columns code, speed
#' @param ... Other arguments passed to projection functions
#' @return List with sequence_speed and historical_speed projections
#' @export
future_projections <- function(data_fut,
                               sequence_speed,
                               path_speed,
                               speed_scores,
                               ...) {
  list(
    sequence_speed = future_path_sequence_speed(data_fut, sequence_speed, path_speed, ...),
    historical_speed = future_path_historical_speed(data_fut, speed_scores, path_speed, ...)
  )
}