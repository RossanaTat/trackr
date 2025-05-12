# Helper function for the user to get data from wdi
# To do later if necessary


#' Prepare data for estimation
#'
#'
#'
prep_data <- function(data           = wbstats::wb_data(indicator = match.arg(indicator),lang = "en",country="countries_only"),
                      startyear_data = 2000,
                      code_col       = "iso3c",
                      year_col       = "date",
                      indicator      = "EG.ELC.ACCS.ZS") {

  # Convert to data.table
  dt <- data.table::as.data.table(data)

  # Standardize column names
  data.table::setnames(dt,
                       old         = c(code_col, year_col, indicator),
                       new         = c("code", "year", "y"),
                       skip_absent = FALSE)

  # Keep only relevant columns
  dt <- dt[, .(code, year, y)]

  # Filter years and order
  dt <- dt[year >= startyear_data]

  data.table::setorder(dt,
                       code,
                       year)

  # Compute annualized changes from 5 to 10 years ahead
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

  # Create folds for cross-validation
  folds <- dt |>
    fselect(code) |>
    distinct()

  folds$fold_id <- sample(1:5,
                          size          = nrow(folds),
                          replace       = TRUE)

  res_data    <- joyn::joyn(dt,
                              folds,
                              by         = "code",
                              match_type = "m:1",
                              reportvar  = FALSE,
                              verbose    = FALSE)

  return(res_data)

}
