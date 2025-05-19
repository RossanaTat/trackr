# _____________________________________________ #
# Set params to be used across tests ~~~ ####
# _____________________________________________ #

indicator      = "EG.ELC.ACCS.ZS"
pctlseq        = seq(20,80,20)
speedseq       = c(0.25,0.5,1,2,4)
startyear_data = 2000
start_year     = startyear_evaluation = 2000
end_year       = endyear_evaluation   = 2022
targetyear     = 2030
best           = "high"
floor          = 0
ceiling        = 100
granularity    = 0.1
lambdas        = 0.1*1.148^(0:50)

min = round(
  if_else(is.na(floor),
          min(data_model$initialvalue),
          floor)/granularity)*granularity

max = round(
  if_else(is.na(ceiling),
          max(data_model$initialvalue),
          ceiling)/granularity)*granularity

data_wdi <- wbstats::wb_data(indicator = indicator,lang = "en",country="countries_only") |>
  rename("year" = "date","code" = "iso3c","y"=indicator) |>
  # Only keep relevant columns
  select(year,code,y)

data_fut <- data_wdi |>
  filter(!is.na(y)) |>
  group_by(code) |>
  arrange(year) |>
  filter(row_number()==n()) |>
  # Store the last observation rounded in a new column. To be used as the starting point for future targets
  mutate(y_fut = if_else(row_number()==n(),round(y/granularity)*granularity,NA)) |>
  ungroup() |>
  as.data.table()

# _____________________________________________ #
# Prep future data ~~~ ####
# _____________________________________________ #

test_that("prep fut data works as expected", {

  res <- prep_data_fut()

  # Check that the result is a data.table
  expect_true(data.table::is.data.table(res))

  # Reorder columns to match reference
  setcolorder(res, names(data_fut))

  # Sort both by keys to ensure row order consistency
  setorder(res, code)
  setorder(data_fut, code)

  # Compare the datasets
  expect_equal(res,
               data_fut,
               tolerance = 1e-8,
               ignore_attr = TRUE)

})
