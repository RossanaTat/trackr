
#____________________________ #
### HISTORICAL PATHS ####
#____________________________ #

#' Get historical baseline data
#'
#' Extract and format historical indicator data for each country to establish a baseline (y_his)
#' used to evaluate progress. This function selects data within a given time range,
#' filters out leading missing values, and stores the first valid (non-missing) observation—
#' rounded to a specified granularity—as the baseline value (`y_his`).
#'
#' @inheritParams prep_data
#' @inheritParams predict_changes
#' @param min A numeric value indicating the minimum bound,
#'   typically passed from a previously processed dataset (e.g., `prep_data()` output via `min <- data_model$min`).
#'   This value is stored as an attribute of the returned object for later reference (e.g., in visualizations or simulations).
#'
#' @param max A numeric value indicating the maximum bound,
#'   typically passed from a previously processed dataset (e.g., `prep_data()` output via `max <- data_model$max`).
#'   This value is stored as an attribute of the returned object for later reference (e.g., in visualizations or simulations).
#' @param eval_from The first year to include in the analysis
#' @param eval_to The last year to include in the analysis
#' @return A `data.table` containing the following columns:
#'   \item{code}{Country code (standardized).}
#'   \item{year}{Year of observation.}
#'   \item{y}{Value of the indicator.}
#'   \item{y_his}{First non-missing historical value (rounded to the specified granularity).}
#'
#' @examples
#' \dontrun{
#'   his_data <- get_his_data(indicator = "EG.ELC.ACCS.ZS",
#'                            start_year = 2000,
#'                            end_year = 2022,
#'                            granularity = 0.1)
#' }
#'
#' @export
get_his_data <- function(indicator    = "EG.ELC.ACCS.ZS",
                         data         = wbstats::wb_data(indicator = indicator, lang = "en", country = "countries_only"),
                         code_col     = "iso3c",
                         year_col     = "date",
                         min          = 0,
                         max          = 100,
                         eval_from   = 2000,
                         eval_to     = 2022,
                         granularity  = 0.1) {

  # Input validation
  stopifnot(is.numeric(eval_from),
            is.numeric(eval_to),
            start_year <= eval_to)

  stopifnot(is.numeric(granularity),
            granularity > 0)

  # Convert to data.table
  dt <- qDT(data) #qDT

  # Standardize column names
  data.table::setnames(dt,
                       old         = c(code_col, year_col, indicator),
                       new         = c("code", "year", "y"),
                       skip_absent = FALSE)

  # Keep relevant columns and filter years
  dt <- dt[
    year >= eval_from
  ][
    , .(code, year, y)
  ]

  # Order data
  data.table::setorder(dt, year)

  # Filter to evaluation period and remove leading NAs
  dt <- dt[
    between(year, eval_from, end_year)
  ][
    , cum_nm := cumsum(!is.na(y)), by = code
  ][
    cum_nm != 0
  ]

  # Compute y_his: store rounded first value by group
  dt[, y_his := fifelse(seq_len(.N) == 1, round(y / granularity) * granularity, NA_real_), by = code]

  # Drop cum_nm
  dt[, cum_nm := NULL]

  # Set attributes from external computation
  setattr(dt,
       "min",
       min)

  setattr(dt,
       "max",
       max)

  setorder(dt,
           code, year)

  return(dt)
}

#' Project percentiles paths
#'
#' Simulates year-by-year projected paths of a variable across percentiles,
#' based on historical values and predicted changes.
#'
#' @param data_his A `data.table` containing historical values with variables `code`, `year`, and `y_his`.
#' @inheritParams get_his_data
#' @inheritParams predict_pctls
#' @param sequence_pctl Numeric vector. Sequence of percentiles
#' @param predictions_pctl A `data.table` with predicted changes by `initialvalue` and `pctl`.
#' @param verbose Logical. Whether to print progress messages.
#'
#' @return A `data.table` with projected values `y_his` by `code`, `year`, and `pctl`.
#'
#' @export
old_project_pctls_path <- function(data_his,
                                   eval_from         = 2000,
                                   end_year          = 2022,
                                   granularity       = 0.1,
                                   floor             = 0,
                                   ceiling           = 100,
                                   min               = NULL,
                                   max               = NULL,
                                   sequence_pctl     = seq(20, 80, 20),
                                   predictions_pctl,
                                   verbose           = TRUE) {
  # Input validation
  if (!inherits(data_his, "data.table")) {
    cli::cli_abort("Input data must be a data.table")
  }

  if (is.null(min)) min <- attr(data_his, "min")
  if (is.null(max)) max <- attr(data_his, "max")


  # Create base table: all combinations of code, year, percentile
  path_his_pctl <- as.data.table(expand.grid(
    code = unique(data_his$code),
    year = seq(eval_from, eval_to),
    pctl = sequence_pctl
  ))

  # Merge in the historical y values
  path_his_pctl <- invisible(joyn::joyn(
    x          = path_his_pctl,
    y          = data_his,
    by         = c("code", "year"),
    match_type = "m:1",
    keep       = "left",
    reportvar  = FALSE,
    verbose = FALSE
  ))

  # Initialize y_his with y
  path_his_pctl[, y_his := y]

  # Sort for reliable row-based operations
  setorder(path_his_pctl, code, pctl, year)

  if (verbose) cli::cli_alert_info("Calculating historical percentile paths")

  # Iterate over years, starting from the second
  for (yr in seq(eval_from + 1, eval_to)) {

    #if (verbose) cli::cli_alert_info("Processing year {.strong {yr}}")

    if (verbose) cli::cli_alert_info(
      paste0("Processing year: ",
             cli::col_green("{.strong {yr}}")))


    # Create temporary table of values from previous year
    prev_year_dt <- path_his_pctl[year == yr - 1, .(code, pctl, initialvalue = y_his)]

    # Join with predictions
    updated_dt <- invisible(joyn::joyn(
      x          = prev_year_dt,
      y          = predictions_pctl,
      by         = c("initialvalue", "pctl"),
      match_type = "m:1",
      keep       = "left",
      reportvar  = FALSE,
      verbose    = FALSE
    ))

    # Calculate updated y_his
    updated_dt[, y_his := round((initialvalue + change) / granularity) * granularity]
    updated_dt[, year := yr]

    # Join back the updated values into the main path table
    path_his_pctl <- invisible(joyn::joyn(
      x          = path_his_pctl,
      y          = updated_dt[, .(code, pctl, year, y_his_new = y_his)],
      by         = c("code", "pctl", "year"),
      match_type = "1:1",
      keep       = "left",
      reportvar  = FALSE,
      verbose    = FALSE
    ))

    # Replace old y_his where new ones exist
    path_his_pctl[!is.na(y_his_new), y_his := y_his_new]
    path_his_pctl[, y_his_new := NULL]
  }

  # Keep values within desired min/max bounds
  path_his_pctl <- path_his_pctl[y >= min & y <= max]

  return(path_his_pctl)
}



#' Project speed path
#'
#' Calculates path a country would have taken with various speeds
#'
#' @inheritParams get_speed_path
#' @inheritParams project_pctls_path
#' @param speedseq numeric vector of speed paths to calculate
#'
#'
#' @return A data frame of projected values under different speeds
#'
project_path_speed <- function(data_his,
                               speedseq    = c(0.25,0.5,1,2,4),
                               path_speed,
                               floor       = 0,
                               ceiling     = 100,
                               granularity = 0.1,
                               eval_from  = 2000,
                               eval_to    = 2022,
                               min         = NULL,
                               max         = NULL,
                               best = "high") {

  # Validate input

  if (is.null(min)) {
    min <- attr(data_his, "min")
  }

  if (is.null(max)) {
    max <- attr(data_his, "max")
  }



  data_his <- cross_join(data_his,
                         as.data.frame(speedseq)) |>
    rename("speed" = "speedseq")

  # Create a new data set which will contain the path a country would have taken with various speeds
  path_his_speed <- data_his |>
    filter(!is.na(y_his)) |>
    select(code,
           y_his,
           year,
           speed) |>
    cross_join(path_speed) |>
    mutate(best = best) |>
    filter(if_else(best=="high",
                   y_his<=y,
                   y_his>=y)) |>
    group_by(code,
             speed) |>
    arrange(time) |>
    mutate(year = year + (time-time[1])/speed) |>
    ungroup() |>
    select(-c(y_his,time,best)) |>
    rename("y_his" = "y") |>
    joyn::joyn(data_his,
               match_type="1:1",
               by=c("code","year","speed"),
               reportvar=FALSE,
               verbose = FALSE,
               y_vars_to_keep="y") |>
    group_by(code,
             speed) |>
    arrange(year) |>
    mutate(y_his = zoo::na.approx(y_his,
                                  year,
                                  na.rm=FALSE,
                                  rule=2)) |>
    filter(year %in% seq(eval_from,
                         eval_to,
                         1)) |>
    ungroup() |>
    # Only keep cases where target has not been reached
    filter(between(y,
                   min,
                   max)) |>
    as.data.table()

  return(path_his_speed)


}

#' Wrapper to compute historical paths by percentiles and/or speed
#'
#' @inheritParams predict_changes
#' @param data_his A data.table containing historical values.
#' @param eval_from First year to include in projections.
#' @param eval_to Last year to include in projections.
#' @param predictions_pctl A `data.table` with predicted changes by initialvalue and pctl.
#' @param verbose Logical. Whether to print progress messages (only used in percentile projection).
#' @param speedseq Numeric vector of XXX
#' @param path_speed Data table  with xxx
#'
#' @return A named list with one or both of `percentile_path` and `speed_path`.
#' @export
path_historical <- function(percentiles      = TRUE,
                            speed            = TRUE,
                            data_his,
                            eval_from        = 2000,
                            eval_to          = 2022,
                            granularity      = 0.1,
                            floor            = 0,
                            ceiling          = 100,
                            min              = 0,
                            max              = 100,
                            sequence_pctl    = seq(20, 80, 20),
                            predictions_pctl = NULL,
                            verbose          = TRUE,
                            speedseq         = c(0.25, 0.5, 1, 2, 4),
                            path_speed       = NULL,
                            best = "high") {

  # Default values for min/max
  if (is.null(min)) min <- attr(data_his, "min")
  if (is.null(max)) max <- attr(data_his, "max")

  # Input checks
  if (speed && is.null(path_speed)) {
    cli::cli_abort("{.arg path_speed} must be provided when {.arg speed} is TRUE.")
  }

  if (percentiles && is.null(predictions_pctl)) {
    cli::cli_abort("{.arg predictions_pctl} must be provided when {.arg percentiles} is TRUE.")
  }



  out <- list()

  if (percentiles) {
    out$percentile_path <- project_pctls_path(
      data_his         = data_his,
      eval_from        = eval_from,
      eval_to          = eval_to,
      granularity      = granularity,
      #floor           = floor,
      #ceiling         = ceiling,
      min              = min,
      max              = max,
      sequence_pctl    = sequence_pctl,
      predictions_pctl = predictions_pctl,
      verbose          = verbose
    )
  }

  if (speed) {
    out$speed <- project_path_speed(
      data_his    = data_his,
      speedseq    = speedseq,
      path_speed  = path_speed,
      floor       = floor,
      ceiling     = ceiling,
      granularity = granularity,
      eval_from   = eval_from,
      eval_to     = eval_to,
      min         = min,
      max         = max,
      best        = best
    )
  }

  if (length(out) == 0) {
    cli::cli_abort("At least one of `percentiles` or `speed` must be TRUE.")
  }

  return(out)
}


## TESTING NEW PROJECT PCTLS PATHS ####
# Filter early to only include cases where target is not yet met

#' Project percentiles paths
#'
#' Simulates year-by-year projected paths of a variable across percentiles,
#' based on historical values and predicted changes.
#'
#' @param data_his A `data.table` containing historical values with variables `code`, `year`, and `y_his`.
#' @inheritParams get_his_data
#' @inheritParams predict_pctls
#' @param sequence_pctl Numeric vector. Sequence of percentiles
#' @param predictions_pctl A `data.table` with predicted changes by `initialvalue` and `pctl`.
#' @param verbose Logical. Whether to print progress messages.
#'
#' @return A `data.table` with projected values `y_his` by `code`, `year`, and `pctl`.
#'
#' @export
project_pctls_path <- function(data_his,
                               eval_from      = 2000,
                               eval_to        = 2022,
                               granularity    = 0.1,
                               #floor         = 0,
                               #ceiling       = 100,
                               min            = NULL,
                               max            = NULL,
                               sequence_pctl  = seq(20, 80, 20),
                               predictions_pctl,
                               verbose        = TRUE) {
  # Input validation
  if (!inherits(data_his, "data.table")) {
    cli::cli_abort("Input data must be a data.table")
  }

  if (is.null(min)) {
    min <- attr(data_his, "min")
    data_his[y >= min]
  }

  if (is.null(max)) {
    max <- attr(data_his, "max")
    data_his[y <= max]
  }


  # Step 1: Expand data_his at start_year by pctl
  dt_expanded <- data_his[year == eval_from, .(code, year, y_his)]

  dt_expanded <- dt_expanded[, .(pctl = sequence_pctl),
                             by = .(code, year, y_his)]

  # Step 2: Add rows for future years
  all_years <- seq(eval_from,
                   eval_to)

  dt_expanded <- dt_expanded[, .(year = all_years),
                             by = .(code, pctl)][
    dt_expanded, on = .(code, pctl, year), y_his := i.y_his]

  setorder(dt_expanded,
           code,
           pctl,
           year)

  # Step 3: Project year by year
  for (i in 2:length(all_years)) {

    prev_year <- all_years[i - 1]
    this_year <- all_years[i]


    prev <- dt_expanded[year == prev_year, .(code, pctl, y_his)]

    prev <- joyn::left_join(prev,
                            predictions_pctl,
                            by = c("y_his = initialvalue", "pctl"),
                            relationship = "many-to-many",
                            verbose = FALSE)

    qDT(prev)

    # Apply change and granularity, and floor/ceiling bounds
    prev[, new_y := round((y_his + change) / granularity) * granularity]


    # Assign updated y_his to this year
    dt_expanded[year == this_year,
                y_his := prev[.SD, on = .(code, pctl),
                              x.new_y]]

    setorder(dt_expanded,
             code,
             year)

  }

  dt_expanded <- joyn::left_join(

    dt_expanded,
    data_his[, .(code, year, y)],       # only merge 'y'
    by           = c("code", "year"),
    relationship = "many-to-one",
    verbose      = FALSE,
    reportvar    = FALSE
  )

  return(dt_expanded[])

}
