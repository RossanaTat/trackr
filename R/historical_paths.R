
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
#' @param max A numeric value indicating the maximum bound,
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
#'                            eval_from = 2000,
#'                            eval_to = 2022,
#'                            granularity = 0.1)
#' }
#'
#' @export
get_his_data <- function(indicator    = "EG.ELC.ACCS.ZS",
                         data         = wbstats::wb_data(indicator = indicator, lang = "en", country = "countries_only"),
                         code_col     = "iso3c",
                         year_col     = "date",
                         min          = NULL,
                         max          = NULL,
                         eval_from    = 2000,
                         eval_to      = 2022,
                         granularity  = 0.1) {

  if(is.null(min) || is.null(max)) {
    cli::cli_abort(message = "min and max should be provided")
  }

  # Input validation
  stopifnot(is.numeric(eval_from),
            is.numeric(eval_to),
            eval_from <= eval_to)

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
    between(year, eval_from, eval_to)
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
           code,
           year)

  return(dt)
}


#' Project speed path
#'
#' Calculates path a country would have taken with various speeds
#'
#' @inheritParams get_speed_path
#' @inheritParams project_pctls_path
#' @param sequence_speed numeric vector of speed paths to calculate
#'
#'
#' @return A data table of projected values under different speeds
#'
project_path_speed <- function(data_his,
                               sequence_speed = c(0.25, 0.5, 1, 2, 4),
                               path_speed,
                               granularity    = 0.1,
                               eval_from      = 2000,
                               eval_to        = 2022,
                               min            = NULL,
                               max            = NULL,
                               best           = "high") {

  # Set min and max from attributes if not provided
  if (is.null(min)) min <- attr(data_his, "min")
  if (is.null(max)) max <- attr(data_his, "max")

  # Ensure data_his is copied
  data_his_copy <- copy(data_his)

  # Rename actual y as y_actual -keeping it for later
  setnames(data_his_copy, "y", "y_actual")

  # Cross join with sequence_speed
  speeds <- qDT(sequence_speed)[, k := 1]

  data_his_copy[, k := 1]
  data_his_copy <- joyn::merge(data_his_copy,
                          speeds,
                          by              = "k",
                          allow.cartesian = TRUE,
                          reportvar       = FALSE,
                          verbose         = FALSE)

  # Keep only rows where y_his is not NA
  data_his_copy <- data_his_copy[!is.na(y_his)]

  setnames(data_his_copy,
           old = "sequence_speed",
           new = "speed")

  # Cross join with path_speed

  path_speed_copy <- copy(path_speed)
  path_speed_copy[, k := 1]


  path_his_speed <- joyn::merge(data_his_copy,

                                path_speed_copy,
                                by = "k",
                                allow.cartesian                = TRUE,
                                verbose                        = FALSE,
                                reportvar                      = FALSE)


  path_his_speed[, k := NULL]
  path_his_speed[, best := best]

  # Filter: keep only those where path is improving in right direction
  path_his_speed <- path_his_speed[
    (best == "high" & y_his <= y) |
      (best != "high" & y_his >= y)
  ]


  # Compute projected year
  setorder(path_his_speed, code, speed, time)

  path_his_speed[, year := year + (time - time[1]) / speed,
                   by = .(code, speed)]

  path_his_speed[, y_his := NULL][,
                                  time := NULL][,
                                                best:= NULL]

  setnames(path_his_speed,
           old = c("y", "y_actual"),
           new = c("y_speed", "y"))

  # # Interpolate y_his over time
  setorder(path_his_speed, code, speed, year)

  path_his_speed[,
                 y_his := zoo::na.approx(y_speed, year, na.rm = FALSE, rule = 2),
                 by = .(code, speed)]

  # Keep only evaluation years
  path_his_speed <- path_his_speed[year %in% seq(eval_from, eval_to)]

  # Filter for actual values between min and max
  path_his_speed <- path_his_speed[y_speed >= min, ][y_speed <= max, ]

  if (verbose) {
    if (is.null(path_his_speed)) {
      cli::cli_alert_danger("Computed path dt is empty")
    }
  }

  return(path_his_speed[])
}




#' Wrapper to compute historical paths by percentiles and/or speed
#'
#' @inheritParams predict_changes
#' @param data_his A data.table containing historical values.
#' @param eval_from First year to include in projections.
#' @param eval_to Last year to include in projections.
#' @param changes_pctl A `data.table` with predicted changes by initialvalue and pctl.
#' @param verbose Logical. Whether to print progress messages (only used in percentile projection).
#' @param sequence_speed Numeric vector of speed paths to calculate
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
                            min              = 0,
                            max              = 100,
                            best             = NULL,
                            sequence_pctl    = seq(20, 80, 20),
                            changes_pctl     = NULL,
                            sequence_speed   = c(0.25, 0.5, 1, 2, 4),
                            path_speed       = NULL,
                            verbose          = TRUE) {


  # Input checks
  if (speed && is.null(path_speed)) {
    cli::cli_abort("{.arg path_speed} must be provided when {.arg speed} is TRUE.")
  }

  if (percentiles && is.null(changes_pctl)) {
    cli::cli_abort("{.arg changes_pctl} must be provided when {.arg percentiles} is TRUE.")
  }


  out <- list()

  if (percentiles) {

    out$pctl <- project_pctls_path(
      data_his         = data_his,
      eval_from        = eval_from,
      eval_to          = eval_to,
      granularity      = granularity,
      min              = min,
      max              = max,
      sequence_pctl    = sequence_pctl,
      changes_pctl     = changes_pctl,
      verbose          = verbose
    )
  }

  if (speed) {

    out$speed <- project_path_speed(
      data_his          = data_his,
      sequence_speed    = sequence_speed,
      path_speed        = path_speed,
      granularity       = granularity,
      eval_from         = eval_from,
      eval_to           = eval_to,
      min               = min,
      max               = max,
      best              = best
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
#' @param changes_pctl A `data.table` with predicted changes by `initialvalue` and `pctl`.
#' @param verbose Logical. Whether to print progress messages.
#'
#' @return A `data.table` with projected values `y_his` by `code`, `year`, and `pctl`.
#'
#' @export
project_pctls_path <- function(data_his,
                               eval_from      = 2000,
                               eval_to        = 2022,
                               granularity    = 0.1,
                               min            = NULL,
                               max            = NULL,
                               sequence_pctl  = seq(20, 80, 20),
                               changes_pctl,
                               verbose        = TRUE) {
  # Input validation
  if (!inherits(data_his, "data.table")) {
    cli::cli_abort("Input data must be a data.table")
  }


  data_his[y >= min]

  data_his[y <= max]

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


    prev <- dt_expanded[year == prev_year,
                        .(code, pctl, y_his)]

    prev <- joyn::left_join(prev,
                            changes_pctl,
                            by           = c("y_his = y", "pctl"),
                            relationship = "many-to-many",
                            verbose      = FALSE)

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

  setnames(dt_expanded,
           "y_his",
           "y_pctl")

  return(dt_expanded[])

}
