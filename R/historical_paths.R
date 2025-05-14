
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

  return(dt)
}

#' Project percentiles paths
#'
#' Simulates year-by-year projected paths of a variable across percentiles,
#' based on historical values and predicted changes.
#'
#' @param data_his A `data.table` containing historical values with variables `code`, `year`, and `y_his`.
#' @inheritParams get_his_data
#' @param pctlseq Numeric vector. Sequence of percentiles
#' @param predictions_pctl A `data.table` with predicted changes by `initialvalue` and `pctl`.
#' @param verbose Logical. Whether to print progress messages.
#'
#' @return A `data.table` with projected values `y_his` by `code`, `year`, and `pctl`.
#'
#' @export
project_pctls_path <- function(data_his,
                               start_year,
                               end_year,
                               granularity,
                               pctlseq,
                               predictions_pctl,
                               verbose = TRUE) {
  # Input validation
  if (!inherits(data_his, "data.table")) {
    cli::cli_abort("Input data must be a data table")
  }

  # Create a new dataset which will eventually contain the predicted path from startyear_evaluation to endyear_evaluation.
  path_his_pctl <- expand.grid(code=unique(data_his$code),year=seq(startyear_evaluation,endyear_evaluation,1),pctl=pctlseq) |>
    # Merge in the actual data from WDI
    joyn::joyn(data_his,by=c("code","year"),match_type="m:1",keep="left",reportvar=FALSE)


  # Year-by-year, calculate the percentile paths from the first value observed.
  if (verbose) cli::cli_alert_info("Calculating historical percentile paths")

  n=2
  # Continue iteratively until the end year of evaluation
  while (n+startyear_evaluation-1<=endyear_evaluation) {
    # Year processed concurrently
    print(n+startyear_evaluation-1)
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
  path_his_pctl <- path_his_pctl |>
    filter(between(y,min,max))


  return(path_his_pctl)
}
