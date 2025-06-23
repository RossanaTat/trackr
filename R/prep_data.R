# Helper function for the user to get data from wdi
# To do later if necessary


#' Prepares data for analysis
#'
#' Prepares indicator data for estimation by computing annualized changes over 5–10 year periods, selecting the shortest available spell, balancing countries by number of rows, and assigning fold IDs for cross-validation.
#'
#' @param indicator Character. Indicator code (e.g., `"EG.ELC.ACCS.ZS"`). Defaults to access to electricity.
#' @param data Optional. A data frame with indicator data. If NULL, data is downloaded via `wbstats::wb_data()`.
#' @param startyear_data Integer. Minimum year to include in the data. Defaults to 2000.
#' @param endyear_data Integer. Maximum year to include in the data. Defaults to 2023.
#' @param code_col Character. Name of the column with country codes. Defaults to `"iso3c"`.
#' @param year_col Character. Name of the column with years. Defaults to `"date"`.
#' @param min Numeric (optional). Minimum value of the indicator. If NULL, the minimum is computed from the data and rounded to the nearest multiple of `granularity`.
#' @param max Numeric (optional). Maximum value of the indicator. If NULL, the maximum is computed from the data and rounded to the nearest multiple of `granularity`.
#' @param verbose Logical. If TRUE print messages in console. Default is TRUE
#'
#' @return A `list` with 3 elements: 1. data prepared for estimation, 2. min and 3. max. Min and Max are range limits for expected changes, based on floor/ceiling if provided, otherwise on observed values. Rounded to nearest granularity.
#' @importFrom splitstackshape expandRows
#'
#' @export
prep_data <- function(indicator      = "EG.ELC.ACCS.ZS",
                      data           = wbstats::wb_data(indicator = indicator, lang = "en", country = "countries_only"),
                      code_col       = "iso3c",
                      year_col       = "date",
                      startyear_data = 2000,
                      endyear_data   = 2023,
                      min            = NULL,
                      max            = NULL,
                      granularity    = 0.1,
                      verbose        = TRUE) {

  # ________________________________
  # Start formatting the data ####
  # ________________________________

  dt <- qDT(data)

  setnames(dt,
           old = c(code_col, year_col, indicator),
           new = c("code", "year", "y"),
           skip_absent = FALSE)

  dt <- dt[year >= startyear_data & year <= endyear_data, .(code, year, y)]
  setorder(dt, code, year)

  # _______________________________________________________
  # Compute annualized changes from 5 to 10 years ah.  ####
  # _______________________________________________________

  dt[, paste0("c", 5:10) := lapply(5:10, function(n) (shift(y, type = "lead", n = n) - y) / n), by = code]

  # Reshape to long format
  dt_long <- melt(dt,
                  id.vars       = c("code", "year", "y"),
                  measure.vars  = patterns("^c[5-9]$|^c10$"),
                  variable.name = "duration",
                  value.name    = "change",
                  variable.factor = FALSE)

  dt_long <- dt_long[!is.na(change)]
  dt_long[, duration := as.numeric(gsub("^c", "", duration))]
  dt_long[, year_end := year + duration]
  setnames(dt_long, c("year", "y"), c("year_start", "initialvalue"))

  # Keep only the shortest available spell per (code, year_start)
  setorder(dt_long, code, year_start, duration)
  dt_long <- dt_long[, .SD[1], by = .(code, year_start)]

  # Expand rows to balance representation across countries
  dt_long[, n := .N, by = code]
  dt_long[, expansion := round(max(n) / n)]
  dt_long <- dt_long[rep(1L:.N, times = expansion)]

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

