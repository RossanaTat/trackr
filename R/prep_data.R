# Helper function for the user to get data from wdi
# To do later if necessary


#' Prepares data for analysis
#'
#' Prepares indicator data for estimation by computing annualized changes over 5–10 year periods, selecting the shortest available spell, balancing countries by number of rows, and assigning fold IDs for cross-validation.
#'
#' @param indicator Character. Indicator code or name (e.g., `"EG.ELC.ACCS.ZS"`). Defaults to access to electricity.
#' @param data Optional. A data frame with indicator data. If NULL, data is downloaded via `wbstats::wb_data()`.
#' @param startyear_data Integer. Minimum year to include. Defaults to 2000.
#' @param code_col Character. Name of the column with country codes. Defaults to `"iso3c"`.
#' @param year_col Character. Name of the column with years. Defaults to `"date"`.
#' @param verbose Logical. If TRUE print messages in console. Default is TRUE
#'
#' @return A `list` with 3 elements: 1. data prepared for estimation, 2. min and 3. max. Min and Max are range limits for expected changes, based on floor/ceiling if provided, otherwise on observed values. Rounded to nearest granularity.
#' @importFrom splitstackshape expandRows
#'
#' @export
prep_data <- function(indicator      = "EG.ELC.ACCS.ZS",
                      data           = wbstats::wb_data(indicator = indicator, lang = "en", country = "countries_only"),
                      startyear_data = 2000,
                      floor          = 0,
                      ceiling        = 100,
                      granularity    = 0.1,
                      code_col       = "iso3c",
                      year_col       = "date",
                      verbose = TRUE) {

  # ________________________________
  # Start formatting the data ####
  # ________________________________

  # Convert to data.table
  dt <- data.table::as.data.table(data)

  # Standardize column names
  data.table::setnames(dt,
                       old         = c(code_col, year_col, indicator),
                       new         = c("code", "year", "y"),
                       skip_absent = FALSE)

  dt <- dt |>
    fselect(code, year, y) |>
    fsubset(year >= startyear_data)

  data.table::setorder(dt,
                       code,
                       year)
  # _______________________________________________________
  # Compute annualized changes from 5 to 10 years ah.  ####
  # _______________________________________________________

  # To optimize with collapse

  dt <- dt |>
    fmutate(c5   = (lead(y,5)-y)/5,
             c6  = (lead(y,6)-y)/6,
             c7  = (lead(y,7)-y)/7,
             c8  = (lead(y,8)-y)/8,
             c9  = (lead(y,9)-y)/9,
             c10 = (lead(y,10)-y)/10)

  # Reshape to long format

  dt <- dt |>
    pivot(ids              = c("code", "year", "y"),
          values           = paste0("c", 5:10),
          names            = list(variable = "duration"),
          how              = "longer") |>
    fsubset(!is.na(value)) |>
    frename("year_start"   = "year",
            "change"       = "value",
            "initialvalue" = "y") |>
    fmutate(duration       = as.numeric(gsub("^c", "", duration)),
            year_end       = year_start + duration) |>
    fselect(-duration) |>
    group_by(code,year_start) |>
    # TODO move to collapse
    # The line below imply that we only use 6-year spells if there is no 5-year spell, and only use 7-year spells if there is no 5 and 6-year spell etc.
    # The advantage of doing so is that we rely more consistently on spells of the same length.
    slice(1) |> # TODO move to collapse |>
    ungroup() |>

    # Multiply the rows of countries with little data so they have about as many rows as countries with a lot of data
    fcount(code, name = "n", add = TRUE) |>
    #add_count(code) |>
    fmutate(expansion = round(max(n)/n)) |>
    splitstackshape::expandRows('expansion')

  # _____________________________________________
  # Create folds for cross-validation ####
  # _____________________________________________

  folds <- dt |>
    fselect(code) |>
    distinct()

  folds$fold_id <- sample(1:5,
                          size          = nrow(folds),
                          replace       = TRUE)

  # ________________________________
  # Return ####
  # ________________________________


  res_data    <- joyn::joyn(dt,
                              folds,
                              by         = "code",
                              match_type = "m:1",
                              reportvar  = FALSE,
                              verbose    = FALSE) |>
    as.data.table()

  # Calculate floor/ceiling adjusted bounds
  min_val <- round(
    ifelse(is.na(floor),
           min(dt$initialvalue, na.rm = TRUE),
           floor) / granularity
  ) * granularity

  max_val <- round(
    ifelse(is.na(ceiling),
           max(dt$initialvalue, na.rm = TRUE),
           ceiling) / granularity
  ) * granularity

  return(list(
    data_model  = res_data,
    min = min_val,
    max = max_val
  ))


  if (verbose) cli::cli_alert_success("user data successfuly formatted")

  return(res_data)

}
