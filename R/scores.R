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

  score_pctl <- path_his_pctl |>
    filter(!is.na(y) & between(y,
                               min,
                               max)) |>
    group_by(code, pctl) |>
    arrange(year) |>
    mutate(firstobs = if_else(row_number() == 1, y, NA),
           firstyear = if_else(row_number()==1, year, NA)) |>
    tidyr::fill(firstobs) |>
    tidyr::fill(firstyear) |>
    slice_tail(n = 1) |>
    filter(year-firstyear >= 5) |>
    mutate(evaluationperiod = paste0(firstyear,"-",year)) |>
    ungroup() |>
    select(-c(year,firstyear))

  # Calculates the score for indicators where a low value is better
  if (best=="low") {
    score_pctl <- score_pctl |>
      mutate(pctl = 100 - pctl) |>
      group_by(code) |>
      arrange(pctl) |>
      mutate(score = if_else(y>=y_his & row_number()==1,paste0(0,"-",pctl),NA),
             score = if_else(y<y_his & y>=lead(y_his),paste0(pctl,"-",lead(pctl)),score),
             score = if_else(y<y_his & row_number()==n(),paste0(pctl,"-",100),score)) |>
      ungroup()  |>
      filter(firstobs!=best) |>
      filter(!is.na(score)) |>
      select(code,score,evaluationperiod)
  }

  # Calculate the score for indicators where a high value is better
  if (best=="high") {
    score_pctl <- score_pctl |>
      group_by(code) |>
      arrange(pctl) |>
      mutate(score = if_else(y<=y_his & row_number()==1,paste0(0,"-",pctl),NA),
             score = if_else(y>y_his & y<=lead(y_his),paste0(pctl,"-",lead(pctl)),score),
             score = if_else(y>y_his & row_number()==n(),paste0(pctl,"-",100),score)) |>
      ungroup() |>
      filter(firstobs!=best) |>
      filter(!is.na(score)) |>
      select(code,score,evaluationperiod)
  }

  score_pctl <- as.data.table(score_pctl)

  return(score_pctl)

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


