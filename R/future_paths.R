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

# Calculate the future path for each country based on the percentile curves

future_path_pctls <- function(data_fut,
                              pctlseq     = seq(20,80,20),
                              predictions_pctl,
                              target_year = 2030,
                              granularity = 0.1,
                              min         = NULL,
                              max         = NULL) {

  # Create a new dataset which will contain the predicted path from the last observation to targetyear at all selected percentiles

  path_fut_pctl <- expand.grid(code=unique(data_fut$code),
                               year=seq(min(data_fut$year),
                                        target_year,
                                        1),
                               pctl=pctlseq) |>
    # Merge in the actual data from WDI
    joyn::joyn(data_fut,
               by=c("code","year"),
               match_type="m:1",
               keep="left",
               reportvar=FALSE)

  # Year-by-year, calculate the percentile paths for the last value observed. For future target creation.

  cli::cli_alert_info("Calculating future percentile paths")

  startyear_target = min(path_fut_pctl$year)

  n = startyear_target - min(path_fut_pctl$year) + 1

  # Continue this iteratively until the target year
  while (n+min(path_fut_pctl$year)-1 <=target_year) {
    # Year processed concurrently
    #print(n+min(path_fut_pctl$year)-1)
    if (verbose) cli::cli_alert_info("Processing year {.strong {(n+min(path_fut_pctl$year)-1}}")

    path_fut_pctl <- path_fut_pctl |>
      # Merge in data with predicted changes based on initial levels
      joyn::joyn(predictions_pctl,
                 match_type="m:1",
                 keep="left",
                 by=c("y_fut=initialvalue","pctl"),
                 reportvar=FALSE,
                 verbose=FALSE) |>
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


  path_fut_pctl <- path_fut_pctl |>
    filter(!is.na(y_fut)) |>
    filter(between(y,min,max) | is.na(y))

}
