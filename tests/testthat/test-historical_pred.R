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

# Data model

data_wdi <- wbstats::wb_data(indicator = indicator,lang = "en",country="countries_only") |>
  rename("year" = "date","code" = "iso3c","y"=indicator) |>
  # Only keep relevant columns
  select(year,code,y)

data_his <- data_wdi |>
  filter(between(year,startyear_evaluation,endyear_evaluation)) |>
  group_by(code) |>
  arrange(year) |>
  # Removing rows until non-missing value
  mutate(cum_nm = cumsum(!is.na(y))) |>
  filter(cum_nm!=0) |>
  select(-cum_nm) |>
  # Store the first relevant observation in a new column. To be used as the starting value for evaluating progress
  mutate(y_his = if_else(row_number()==1,round(y/granularity)*granularity,NA)) |>
  ungroup() |>
  as.data.table()

data_model <- data_wdi  |>
  # Remove data not to be used for modelling
  filter(year>=startyear_data) |>
  group_by(code) |>
  arrange(year) |>
  mutate(c5  = (lead(y,5)-y)/5,
         c6  = (lead(y,6)-y)/6,
         c7  = (lead(y,7)-y)/7,
         c8  = (lead(y,8)-y)/8,
         c9  = (lead(y,9)-y)/9,
         c10 = (lead(y,10)-y)/10) |>
  tidyr::pivot_longer(c5:c10,names_to="duration") |>
  rename("year_start" = "year","change" = "value", "initialvalue" = "y") |>
  filter(!is.na(change)) |>
  mutate(duration = as.numeric(substr(duration, 2, nchar(duration))),
         year_end = year_start+duration) |>
  select(-duration) |>
  group_by(code,year_start) |>
  slice(1) |>
  ungroup()

data_model <- data_model |>
  add_count(code) |>
  mutate(expansion = round(max(n)/n)) |>
  splitstackshape::expandRows('expansion')

folds <- data_model |>
  select(code) |>
  distinct()

folds$fold_id <- sample(1:5,size=nrow(folds),
                        replace=TRUE)

data_model    <- joyn::joyn(data_model,
                            folds,
                            by="code",
                            match_type="m:1",
                            reportvar=FALSE,
                            verbose=FALSE)

min = round(
  if_else(is.na(floor),
          min(data_model$initialvalue),
          floor)/granularity)*granularity

max = round(
  if_else(is.na(ceiling),
          max(data_model$initialvalue),
          ceiling)/granularity)*granularity

# _____________________________________________ #
# Get historical data ~~~ ####
# _____________________________________________ #

expected_data_his <- data_wdi |>
  filter(between(year,startyear_evaluation,endyear_evaluation)) |>
  group_by(code) |>
  arrange(year) |>
  # Removing rows until non-missing value
  mutate(cum_nm = cumsum(!is.na(y))) |>
  filter(cum_nm!=0) |>
  select(-cum_nm) |>
  # Store the first relevant observation in a new column. To be used as the starting value for evaluating progress
  mutate(y_his = if_else(row_number()==1,round(y/granularity)*granularity,NA)) |>
  ungroup() |>
  as.data.table()

test_that("get_his_data works as expected", {

  result <- get_his_data(
    indicator   = indicator,
    #data        = data_wdi,
    start_year  = startyear_evaluation,
    end_year    = endyear_evaluation,
    granularity = granularity
  )

  # Basic structure checks
  expect_s3_class(result,
                  "data.table")
  expect_named(result,
               c("code", "year", "y", "y_his"))
  expect_equal(nrow(result),
               nrow(expected_data_his))

  # Drop attributes to ensure clean comparison
  result_clean <- copy(result)
  expected_clean <- copy(expected_data_his)
  setattr(result_clean$y_his,
          "label",
          NULL)

  # Value checks
  expect_equal(result_clean$code,
               expected_clean$code)

  expect_equal(result_clean$year,
               expected_clean$year)

  expect_equal(result_clean$y,
               expected_clean$y)

  expect_equal(result_clean$y_his,
               expected_clean$y_his,
               tolerance = 1e-6)

})

# _____________________________________________ #
# Percentiles paths ~~~ ####
# _____________________________________________ #

# ----------- Expected ----------------------- ####

fit_pctl <- gcrq(change ~ ps(initialvalue,lambda=lambdas), foldid=data_model$fold_id, tau=pctlseq/100, data=data_model)

# Create data set with the expected changes as a function of initial level.
predictions_pctl <- as.data.frame(charts(fit_pctl,
                                                  k=seq(min,max,granularity))) |>
  mutate(initialvalue = round(seq(min,max,granularity)/granularity)*granularity) |>
  tidyr::pivot_longer(-initialvalue,names_to="pctl",
                      values_to="change") |>
  mutate(pctl = 100*as.numeric(pctl)) |>
  rowwise() |>
  # Expected changes can never give an outcome lower than the floor
  mutate(change = if_else(!is.na(floor),max(change,floor-initialvalue),change),
         # Expected changes can never give an outcome higher than the ceiling
         change = if_else(!is.na(ceiling),min(change,ceiling-initialvalue),change)) |>
  ungroup() |>
  as.data.table()

# Create a new dataset which will eventually contain the predicted path from startyear_evaluation to endyear_evaluation.
path_his_pctl <- expand.grid(code=unique(data_his$code),year=seq(startyear_evaluation,endyear_evaluation,1),pctl=pctlseq) |>
  # Merge in the actual data from WDI
  joyn::joyn(data_his,by=c("code","year"),match_type="m:1",keep="left",reportvar=FALSE)


# Year-by-year, calculate the percentile paths from the first value observed.
print("Calculating historical percentile paths")
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
path_his_pctl <- as.data.table(path_his_pctl)[y >= min & y <= max]

test_that("project_pctls_path works as expected", {

  # Run the function
  result <- project_pctls_path(
    data_his = get_his_data(min = 0, max = 100),
    start_year = startyear_evaluation,
    end_year = endyear_evaluation,
    granularity = granularity,
    pctlseq = pctlseq,
    predictions_pctl = predictions_pctl,
    verbose = FALSE
  )

  # Check type
  expect_s3_class(result, "data.table")

  # Check column names
  expected_cols <- c("code", "year", "pctl", "y_his")
  expect_true(all(expected_cols %in% colnames(result)))

  # Check number of rows (should match the filtered manual result)
  expect_equal(nrow(result), nrow(path_his_pctl))

  # Check that all values match exactly
  expect_equal(result[order(code, year, pctl)],
               path_his_pctl[order(code, year, pctl)],
               ignore_attr = TRUE)
})


# _____________________________________________ #
# Speed paths ~~~ ####
# _____________________________________________ #


