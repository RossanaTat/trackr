#' Calculate percentile-based score over a historical evaluation period
#'
#' Computes countries’ percentile progress scores over a user-specified evaluation period.
#'
#' @param path_his_pctl A data frame containing columns: `code`, `year`, `y`, `y_pctl`, and `pctl`
#' @param best Character indicating whether higher (`"high"`) or lower (`"low"`) values are better
#' @inheritParams prep_data
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

  dt <- copy(path_his_pctl)

  # Filter out NA and optionally keep only values within range
  dt <- dt[!is.na(y) & between(y, min, max)]

  # Sort and create first observation & first year
  setorder(dt, code, pctl, year)
  dt[, firstobs := y[1L], by = .(code, pctl)]
  dt[, firstyear := year[1L], by = .(code, pctl)]

  # Keep only the last observation in each group
  dt <- dt[, .SD[.N], by = .(code, pctl)]

  # Require at least 5 years evaluation
  dt <- dt[year - firstyear >= 5]
  dt[, evaluationperiod := paste0(firstyear, "-", year)]
  dt[, c("year", "firstyear") := NULL]

  # Order by code and pctl after flipping
  setorder(dt, code, pctl)

  # TODO:
  #### Convert to data table ####

  if (best=="low") {
    dt <- dt |>
      mutate(pctl = 100 - pctl) |>
      group_by(code) |>
      arrange(pctl) |>
      mutate(score = if_else(y>=y_pctl & row_number()==1,paste0(0,"-",pctl),NA),
             score = if_else(y<y_pctl & y>=lead(y_pctl),paste0(pctl,"-",lead(pctl)),score),
             score = if_else(y<y_pctl & row_number()==n(),paste0(pctl,"-",100),score)) |>
      ungroup()  |>
      filter(firstobs!=best) |>
      filter(!is.na(score)) |>
      select(code,score,evaluationperiod)
  }

  # Calculate the score for indicators where a high value is better
  if (best=="high") {
    dt <- dt |>
      group_by(code) |>
      arrange(pctl) |>
      mutate(score = if_else(y<=y_pctl & row_number()==1,paste0(0,"-",pctl),NA),
             score = if_else(y>y_pctl & y<=lead(y_pctl),paste0(pctl,"-",lead(pctl)),score),
             score = if_else(y>y_pctl & row_number()==n(),paste0(pctl,"-",100),score)) |>
      ungroup() |>
      filter(firstobs!=best) |>
      filter(!is.na(score)) |>
      select(code,score,evaluationperiod)
  }


  dt <- qDT(dt)

  # Keep only rows with assigned score
  dt <- dt[!is.na(score), .(code, score, evaluationperiod)]

  return(dt)
}

#' Calculate speed-based score over a historical evaluation period
#'
#' Computes countries’ speed of progress over a user-specified evaluation period.
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

  # Step 1 — Filter on y_speed and round
  path_his_speed <- path_his_speed[
    !is.na(y_speed) & between(y_speed, min, max),
    .(code, year, y = round(y_speed / granularity) * granularity)
  ]

  # Step 2 — Order within groups
  setorder(path_his_speed, code, year)

  # Step 3 — Add y_start and year_start per group
  path_his_speed[, `:=`(
    y_start    = first(y),
    year_start = first(year)
  ), by = code]

  # Step 4 — Keep only the last row per country + at least 5-year span
  last_rows <- path_his_speed[
    , .SD[.N], by = code
  ][(year - year_start) >= 5]

  # Step 5 — Join y_end (last y) with path_speed → time_end
  last_rows[, y := y]
  join1 <- joyn::joyn(
    last_rows,
    path_speed,
    by         = "y",
    match_type = "m:1",
    reportvar  = FALSE,
    keep       = "left",
    verbose    = FALSE
  )
  setnames(join1, "time", "time_end")
  join1[, y := NULL]

  # Step 6 — Join y_start with path_speed → time_start
  join1[, y := y_start]
  join2 <- joyn::joyn(
    join1,
    path_speed,
    by         = "y",
    match_type = "m:1",
    reportvar  = FALSE,
    keep       = "left",
    verbose    = FALSE
  )
  setnames(join2, "time", "time_start")
  join2[, y := NULL]

  # Step 7 — Compute score + evaluation period
  result <- join2[
    , .(
      code,
      score            = (time_end - time_start) / (year - year_start),
      evaluationperiod = paste0(year_start, "-", year)
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
    if (nrow(path_his_pctl) == 0)  cli::cli_abort("Percentile-based scoring selected, but `path_his_pctl` was not provided or is empty")

    out$pctl <- get_scores_pctl(path_his_pctl = path_his_pctl,
                                best          = best,
                                min           = min,
                                max           = max)

    if (verbose) cli::cli_alert_success("Percentile-based scores calculated successfully.")
  }

  if (speed) {

    if (nrow(path_his_speed) == 0 || nrow(path_speed) == 0) {

      cli::cli_abort("Speed-based scoring selected, but `path_his_speed` and/or `path_speed` were not provided or are empty.")
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

