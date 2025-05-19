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
                            min = 0,
                            max = 100) {

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
    (best == "low" & y >= y_his & .I == .I[1L]) | (best == "high" & y <= y_his & .I == .I[1L]),
    paste0(0, "-", pctl),
    NA_character_
  ), by = code]

  dt[, score := fifelse(
    (best == "low" & y < y_his & y >= shift(y_his, type = "lead")) |
      (best == "high" & y > y_his & y <= shift(y_his, type = "lead")),
    paste0(pctl, "-", shift(pctl, type = "lead")),
    score
  ), by = code]

  dt[, score := fifelse(
    (best == "low" & y < y_his & .I == .I[.N]) |
      (best == "high" & y > y_his & .I == .I[.N]),
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
#' @param path_his_speed Data frame with historical values (`code`, `year`, `y`).
#' @param path_speed Lookup table mapping `y` to percentiles (`y`, `time`).
#' @param min Minimum valid value for `y`. Default is 0.
#' @param max Maximum valid value for `y`. Default is 100.
#' @param granularity Rounding precision for `y`. Default is 0.1.
#'
#' @return A data.table with `code`, `score`, and `evaluationperiod`.
#'
#' @export
get_scores_speed <- function(path_his_speed,
                             path_speed,
                             min = 0,
                             max = 100,
                             granularity = 0.1){

  score_speed <- path_his_speed |>
    select(code,year,y) |>
    filter(!is.na(y) & between(y,min,max)) |>
    mutate(y=round(y/granularity)*granularity) |>
    group_by(code) |>
    arrange(year) |>
    mutate(y_start = first(y),
           year_start = first(year))|>
    filter(row_number()==n() & year-year_start>=5) |>
    ungroup() |>
    joyn::joyn(path_speed,by="y",match_type="m:1",reportvar=FALSE,keep="left",verbose=FALSE) |>
    rename("y_end" = "y", "y" = "y_start","time_end" = "time") |>
    joyn::joyn(path_speed,by="y",match_type="m:1",reportvar=FALSE,keep="left",verbose=FALSE) |>
    rename("time_start" = "time") |>
    mutate(score = (time_end-time_start)/(year-year_start)) |>
    mutate(evaluationperiod = paste0(year_start,"-",year)) |>
    select(code,score,evaluationperiod) |>
    filter(!is.na(score))

  score_speed <- score_speed |> as.data.table()


  return(score_speed)
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
get_scores <- function(speed = FALSE,
                       pctl = FALSE,
                       path_his_pctl = NULL,
                       best = "high",
                       path_his_speed = NULL,
                       path_speed = NULL,
                       min = 0,
                       max = 100,
                       granularity = 0.1) {
  if (!speed && !pctl) {
    stop("At least one of `speed` or `pctl` must be TRUE.")
  }

  out <- list()

  if (pctl) {
    if (is.null(path_his_pctl)) stop("`path_his_pctl` must be provided for percentile-based score.")
    out$pctl <- get_scores_pctl(path_his_pctl = path_his_pctl,
                                best = best,
                                min = min,
                                max = max)
  }

  if (speed) {
    if (is.null(path_his_speed) || is.null(path_speed)) {
      stop("Both `path_his_speed` and `path_speed` must be provided for speed-based score.")
    }
    out$speed <- get_scores_speed(path_his_speed = path_his_speed,
                                  path_speed = path_speed,
                                  min = min,
                                  max = max,
                                  granularity = granularity)
  }

  return(out)
}

# Data table version #### TEST ################################
