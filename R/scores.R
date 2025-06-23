#' Calculate percentile-based score over a historical evaluation period
#'
#' Computes a score based on how a country's latest value compares to its past performance,
#' using percentiles (`pctl`) over a minimum 5-year period.
#'
#' @param path_his_pctl A data frame containing columns: `code`, `year`, `y`, `y_his`, and `pctl`
#' @param best Character indicating whether higher (`"high"`) or lower (`"low"`) values are better
#' @param min Numeric. Minimum acceptable value for `y` (default is 0)
#' @param max Numeric. Maximum acceptable value for `y` (default is 100)
#'
#' @return A data.table with columns: `code`, `score` (as a percentile range), and `evaluationperiod`
#'
#' @examples
#' # get_scores_pctl(data, best = "low")
#'
#' @export
get_scores_pctl <- function(path_his_pctl,
                            best = "high",
                            min  = NULL,
                            max  = NULL) {

  if (!data.table::is.data.table(path_his_pctl)) {
    cli::cli_abort("{.arg path_his_pctl} must be a {.cls data.table}.")
  }

  dt <- copy(path_his_pctl)  # avoid modifying input in-place

  # Filter out NA and keep only rows within the desired range
  dt <- dt[!is.na(y) & between(y, min, max)]

  # Create firstobs and firstyear per group
  setorder(dt, code, pctl, year)

  dt[, firstobs := y[1L], by = .(code, pctl)]

  dt[, firstyear := year[1L], by = .(code, pctl)]

  # Keep only the last observation in the group
  dt <- dt[, .SD[.N], by = .(code, pctl)]

  # Filter for evaluation period of at least 5 years
  dt <- dt[year - firstyear >= 5]
  dt[, evaluationperiod := paste0(firstyear, "-", year)]
  dt[, c("year", "firstyear") := NULL]

  # Compute score depending on whether higher or lower is better
  if (best == "low") {
    dt[, pctl := 100 - pctl]
  }

  setorder(dt, code, pctl)

  dt[, score := fifelse(
    (best == "low" & y >= y_pctl & .I == .I[1L]) | (best == "high" & y <= y_pctl & .I == .I[1L]),
    paste0(0, "-", pctl),
    NA_character_
  ), by = code]

  dt[, score := fifelse(
    (best == "low" & y < y_pctl & y >= shift(y_pctl, type = "lead")) |
      (best == "high" & y > y_pctl & y <= shift(y_pctl, type = "lead")),
    paste0(pctl, "-", shift(pctl, type = "lead")),
    score
  ), by = code]

  dt[, score := fifelse(
    (best == "low" & y < y_pctl & .I == .I[.N]) |
      (best == "high" & y > y_pctl & .I == .I[.N]),
    paste0(pctl, "-", 100),
    score
  ), by = code]

  dt <- dt[firstobs != best & !is.na(score), .(code, score, evaluationperiod)]

  return(dt)
}


#' Calculate speed-based score over time
#'
#' Computes a score based on the average annual change in percentile rank over at least 5 years.
#'
#' @param path_his_speed Result of `project_pctls_path()`
#' @param path_speed Result of `get_speed_path()`
#' @inheritParams prep_data
#' @param granularity Rounding precision for `y`. Default is 0.1.
#'
#' @return A data.table with `code`, `score`, and `evaluationperiod`.
#'
#' @export
get_scores_speed <- function(path_his_speed,
                             path_speed,
                             min         = NULL,
                             max         = NULL,
                             granularity = 0.1) {

  # Ensure data.table
  path_his_speed <- as.data.table(path_his_speed)
  path_speed     <- as.data.table(path_speed)

  # Filter, round, and keep relevant columns
  path_his_speed <- path_his_speed[
    !is.na(y) & between(y, min, max),
    .(code, year, y = round(y / granularity) * granularity)
  ]

  # Compute per-code start and end
  path_his_speed   <- path_his_speed[order(code, year)]

  summary_dt       <- path_his_speed[
    , .(
      y_start = first(y),
      year_start = first(year),
      y_end = last(y),
      year_end = last(year)
    ),
    by = code
  ][(year_end - year_start) >= 5]

  # First join: match y_end with path_speed$y
  summary_dt_join1 <- copy(summary_dt)

  summary_dt_join1[, y := y_end]  # Rename for join
  join1            <- joyn::joyn(
    summary_dt_join1,
    path_speed,
    by = "y",
    match_type = "m:1",
    reportvar = FALSE,
    keep = "left",
    verbose = FALSE
  )
  setnames(join1, "time", "time_end")
  join1[, y := NULL]  # Clean up temp column

  # Second join: match y_start with path_speed$y
  join1[, y := y_start]
  join2            <- joyn::joyn(
    join1,
    path_speed,
    by = "y",
    match_type = "m:1",
    reportvar = FALSE,
    keep = "left",
    verbose = FALSE
  )
  setnames(join2, "time", "time_start")
  join2[, y := NULL]  # Clean up again

  # Compute score
  result           <- join2[
    , .(
      code,
      score = (time_end - time_start) / (year_end - year_start),
      evaluationperiod = paste0(year_start, "-", year_end)
    )
  ][!is.na(score)]

  return(result[])
}

#' Wrapper to calculate percentile-based and/or speed-based scores
#'
#' Calls `get_scores_pctl()` and/or `get_scores_speed()` depending on input.
#'
#' @param speed Logical. If TRUE, compute speed-based score.
#' @param pctl Logical. If TRUE, compute percentile-based score.
#' @param path_his_pctl Data for `get_scores_pctl()`.
#' @param best "high" or "low", for `get_scores_pctl()`.
#' @param path_his_speed Data for `get_scores_speed()`.
#' @param path_speed Lookup for `get_scores_speed()`.
#' @param min Minimum value for both methods (default 0).
#' @param max Maximum value for both methods (default 100).
#' @param granularity Granularity for speed-based method (default 0.1).
#'
#' @return A list with `pctl` and/or `speed` elements (data.tables).
#'
#' @export
get_scores <- function(speed          = FALSE,
                       pctl           = FALSE,
                       path_his_pctl  = NULL,
                       best           = "high",
                       path_his_speed = NULL,
                       path_speed     = NULL,
                       min            = 0,
                       max            = 100,
                       granularity    = 0.1,
                       verbose        = TRUE) {
  if (!speed && !pctl) {
    stop("At least one of `speed` or `pctl` must be TRUE.")
  }

  out <- list()

  if (pctl) {
    if (is.null(path_his_pctl))  stop("Percentile-based scoring selected, but `path_his_pctl` was not provided.")

    out$pctl <- get_scores_pctl(path_his_pctl = path_his_pctl,
                                best          = best,
                                min           = min,
                                max           = max)

    if (verbose) cli::cli_alert_success("Percentile-based scores calculated successfully.")
  }

  if (speed) {

    if (is.null(path_his_speed) || is.null(path_speed)) {

      stop("Speed-based scoring selected, but `path_his_speed` and/or `path_speed` were not provided.")
    }
    out$speed <- get_scores_speed(path_his_speed = path_his_speed,
                                  path_speed     = path_speed,
                                  min            = min,
                                  max            = max,
                                  granularity    = granularity)

    if (verbose) cli::cli_alert_success("Speed-based scores calculated successfully.")

  }


  return(out)
}

