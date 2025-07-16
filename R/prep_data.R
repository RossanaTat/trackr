#' Prepares data for analysis
#'
#' Prepares indicator data for estimation by computing annualized changes over 5–10 year periods, selecting the shortest available spell, balancing countries by number of rows, and assigning fold IDs for cross-validation.
#'
#' @param indicator Character. Name of the column with the values of the indicator data
#' @param data Optional. A data frame with indicator data.
#' @param startyear_data Integer. First year to include in the modelling of typical experiences observed. If not provided, defaults to the minimum value of year_col.
#' @param endyear_data Integer. Last year to include in the modelling of typical experiences observed. If not provided, defaults to the maximum value of year_col.
#' @param code_col Character. Name of the column with country codes (or other groupings used to calculate and compare progress)
#' @param year_col Character. Name of the column with years.
#' @param min Numeric (optional). Minimum value of the indicator. If NULL, the minimum is computed from the data and rounded to the nearest multiple of `granularity`.
#'            Will be overruled by support if that reflects a higher minimum value
#' @param max Numeric (optional). Maximum value of the indicator. If NULL, the maximum is computed from the data and rounded to the nearest multiple of `granularity`. Will be overruled by support if that reflects a lower maximum value.
#' @param verbose Logical. If TRUE print messages in console. Default is TRUE
#' @param granularity Numeric. Granularity in outcome variable. All outcome values are rounded to the nearest value reflecting this granularity. For example, if 1 is chosen, then all outcome values are rounded to the nearest integer. Default is 0.1. The lower granularity, the more precise the estimated paths but the longer it takes to run the code.
#'
#'
#' @return A `list` with 3 elements: 1. data prepared for estimation, 2. min and 3. max. Min and Max are range limits for expected changes, based on floor/ceiling if provided, otherwise on observed values. Rounded to nearest granularity.
#' @importFrom splitstackshape expandRows
#'
#' @export
prep_data <- function(indicator      = NULL,
                      data           = wbstats::wb_data(indicator = indicator, lang = "en", country = "countries_only"),
                      code_col       = NULL,
                      year_col       = NULL,
                      startyear_data = 2000,
                      endyear_data   = 2023,
                      min            = NULL,
                      max            = NULL,
                      granularity    = 0.1,
                      verbose        = TRUE) {

  if (is.null(indicator)) {
    cli::cli_abort("indicator name must be provided")
  }

  # ________________________________
  # Start formatting the data ####
  # ________________________________

  dt <- qDT(data)

  setnames(dt,
           old         = c(code_col, year_col, indicator),
           new         = c("code", "year", "y"),
           skip_absent = FALSE)

  dt <- dt[year >= startyear_data & year <= endyear_data, .(code, year, y)]

  setorder(dt,
           code,
           year)

  # _______________________________________________________
  # Compute annualized changes from 5 to 10 years ah.  ####
  # _______________________________________________________


  dt[, paste0("c", 5:10) := Map(function(n) (shift(y,
                                                   type = "lead",
                                                   n = n) - y) / n, 5:10)]


  # Reshape to long format
  dt_long <- melt(
    dt,
    id.vars = c("code", "year", "y"),
    measure.vars = paste0("c", 5:10),
    variable.name = "duration",
    value.name = "change"
  )

  setnames(dt_long,
           old = c("year", "y"),
           new = c("year_start", "initialvalue"))

  dt_long[, duration := as.numeric(gsub("^c", "", as.character(duration)))][,
            year_end := year_start + duration]


  setorder(dt_long, code, year_start, duration)

  dt_long <- dt_long[,
                     .SD[1], by = .(code, year_start)]

  dt_long <- dt_long[!is.na(change)]
  dt_long[, duration := NULL]
  dt_long[, n := .N, by = code]

  max_n <- dt_long[, max(n)]
  dt_long[, expansion := round(max_n / n)]

  dt_long <- dt_long |> splitstackshape::expandRows('expansion')

  # _____________________________________________
  # Create folds for cross-validation ####
  # _____________________________________________

  folds <- unique(dt_long[, .(code)])
  folds[, fold_id := sample(1:5, .N, replace = TRUE)]


  # _____________________________________________
  # Handle min and max ####
  # _____________________________________________

  min_val <- if (!is.null(min)) {
    round(min / granularity) * granularity
  } else {
    round(min(dt_long$initialvalue, na.rm = TRUE) / granularity) * granularity
  }

  max_val <- if (!is.null(max)) {
    round(max / granularity) * granularity
  } else {
    round(max(dt_long$initialvalue, na.rm = TRUE) / granularity) * granularity
  }

  # ________________________________
  # Return ####
  # ________________________________

  res_data <- joyn::joyn(dt_long,
                         folds,
                         by         = "code",
                         match_type = "m:1",
                         reportvar  = FALSE,
                         verbose    = FALSE)

  if (verbose) cli::cli_alert_success("User data successfully formatted")

  invisible(list(
    data_model = res_data,
    min        = min_val,
    max        = max_val,
    indicator  = indicator
  ))
}
