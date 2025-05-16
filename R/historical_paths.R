
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
#' @param start_year The first year to include in the analysis
#' @param end_year The last year to include in the analysis
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
                         min          = NULL,
                         max          = NULL,
                         start_year   = 2000,
                         end_year     = 2022,
                         granularity  = 0.1) {

  # Input validation
  stopifnot(is.numeric(start_year),
            is.numeric(end_year),
            start_year <= end_year)

  stopifnot(is.numeric(granularity),
            granularity > 0)

  # Convert to data.table
  dt <- data.table::as.data.table(data)

  # Standardize column names
  data.table::setnames(dt,
                       old         = c(code_col, year_col, indicator),
                       new         = c("code", "year", "y"),
                       skip_absent = FALSE)

  # Keep relevant columns and filter years
  dt <- dt[
    year >= start_year
  ][
    , .(code, year, y)
  ]

  # Order data
  data.table::setorder(dt, year)

  # Filter to evaluation period and remove leading NAs
  dt <- dt[
    between(year, start_year, end_year)
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
#' @param pctlseq Numeric vector. Sequence of percentiles
#' @param predictions_pctl A `data.table` with predicted changes by `initialvalue` and `pctl`.
#' @param verbose Logical. Whether to print progress messages.
#'
#' @return A `data.table` with projected values `y_his` by `code`, `year`, and `pctl`.
#'
#' @export
project_pctls_path <- function(data_his,
                               start_year  = 2000,
                               end_year    = 2022,
                               granularity = 0.1,
                               floor       = 0,
                               ceiling     = 100,
                               min         = NULL,
                               max         = NULL,
                               pctlseq     = seq(20,80,20),
                               predictions_pctl,
                               verbose     = TRUE) {
  # Input validation
  if (!inherits(data_his, "data.table")) {
    cli::cli_abort("Input data must be a data table")
  }


  if (is.null(min)) {
    min <- attr(data_his, "min")
  }

  if (is.null(max)) {
    max <- attr(data_his, "max")
  }


  # Create a new dataset which will eventually contain the predicted path from startyear_evaluation to endyear_evaluation.
  path_his_pctl <- as.data.table(expand.grid(code = unique(data_his$code),year=seq(start_year,
                                                                   end_year,
                                                                   1),
                               pctl = pctlseq)) |>
    # Merge in the actual data from WDI
    joyn::joyn(data_his,
               by=c("code","year"),
               match_type="m:1",
               keep="left",
               reportvar=FALSE)


  # Year-by-year, calculate the percentile paths from the first value observed.
  if (verbose) cli::cli_alert_info("Calculating historical percentile paths")

  n=2
  # Continue iteratively until the end year of evaluation
  while (n+start_year-1 <= end_year) {
    # Year processed concurrently
    if (verbose) cli::cli_alert_info("Processing year {.strong {n + start_year - 1}}")
    path_his_pctl <- path_his_pctl |>
      # Merge in data with predicted changes based on initial levels
      joyn::joyn(predictions_pctl,match_type="m:1",keep="left",by=c("y_his=initialvalue","pctl"),reportvar=FALSE, verbose=FALSE) |>
      group_by(code,pctl) |>
      arrange(year) |>
      # Calculate new level based on the predicted changes.
      mutate(y_his = if_else(row_number()==n  & !is.na(lag(y_his)),round((lag(y_his)+lag(change))/granularity)*granularity,y_his)) |>
      ungroup() |>
      select(-change)
    # Move to next year
    n=n+1
  }

  # Only keep cases where target has not been reached
  path_his_pctl <- as.data.table(path_his_pctl)[y >= min & y <= max]


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
                               speedseq = c(0.25,0.5,1,2,4),
                               path_speed,
                               floor   = 0,
                               ceiling = 100,
                               granularity = 0.1,
                               start_year = 2000,
                               end_year = 2022,
                               min = NULL,
                               max = NULL) {

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
    mutate(bst = best) |>
    filter(if_else(bst=="high",
                   y_his<=y,
                   y_his>=y)) |>
    group_by(code,
             speed) |>
    arrange(time) |>
    mutate(year = year + (time-time[1])/speed) |>
    ungroup() |>
    select(-c(y_his,time,bst)) |>
    rename("y_his" = "y") |>
    joyn::joyn(data_his,
               match_type="1:1",
               by=c("code","year","speed"),
               reportvar=FALSE,
               y_vars_to_keep="y") |>
    group_by(code,
             speed) |>
    arrange(year) |>
    mutate(y_his = zoo::na.approx(y_his,
                                  year,
                                  na.rm=FALSE,
                                  rule=2)) |>
    filter(year %in% seq(start_year,
                         end_year,
                         1)) |>
    ungroup() |>
    # Only keep cases where target has not been reached
    filter(between(y,
                   min,
                   max)) |>
    as.data.table()

  return(path_his_speed)


}
