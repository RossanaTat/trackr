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
  ungroup()

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

# _____________________________________________ #
# Speed predictions ~~~ ####
# _____________________________________________ #


fit_speed <- gcrq(change ~ ps(initialvalue,lambda=lambdas),
                  foldid=data_model$fold_id, tau=0.5, data=data_model)

# Create dataset with the expected changes as a function of initial level.
expected_predictions <- as.data.frame(charts(fit_speed, k=seq(min,max,granularity))) |>
  mutate(initialvalue = round(seq(min,max,granularity)/granularity)*granularity) |>
  rename("change" = "V1") |>
  rowwise() |>
  # Expected changes can never give an outcome lower than the floor
  mutate(change = if_else(!is.na(floor),max(change,floor-initialvalue),change),
         # Expected changes can never give an outcome higher than the ceiling
         change = if_else(!is.na(ceiling),min(change,ceiling-initialvalue),change)) |>
  ungroup() |>
  as.data.table()

# --------------------------- . --------------------------- ####

test_that("predict_speed returns accurate predictions", {

  # 1. Run the function
  my_predictions <- predict_speed(data_model = data_model)

  # Structure check
  expect_true(all(c("initialvalue", "change") %in% names(my_predictions)))
  expect_equal(sort(names(my_predictions)),
               sort(names(expected_predictions)))

  # 4. Value check (numerical tolerance)
  setorder(my_predictions,
           initialvalue)
  setorder(expected_predictions,
           initialvalue)

  expect_equal(my_predictions$initialvalue,
               expected_predictions$initialvalue)

  expect_equal(my_predictions$change,
               expected_predictions$change, tolerance = 1e-6)
})

test_that("predict_speed handles empty input gracefully", {

  # empty_input <- data_model[0, ]
  #
  # expect_error(predict_speed(empty_input),
  #              NA)  # No error should be thrown

})

test_that("predict_speed returns within bounds", {

  my_predictions <- predict_speed(data_model)

  # Check that predictions do not exceed ceiling or go below floor
  expect_true(all(my_predictions$initialvalue + my_predictions$change <= ceiling + 1e-6))

  expect_true(all(my_predictions$initialvalue + my_predictions$change >= floor - 1e-6))
})

test_that("predict_speed returns a data.table", {

  my_predictions <- predict_speed(data_model)
  expect_s3_class(my_predictions, "data.table")

})

test_that("predict_speed applies granularity correctly to initialvalue", {

  gran <- 0.2
  preds <- predict_speed(data_model,
                         granularity = gran,
                         verbose = FALSE)

  ivals <- preds$initialvalue
  # Generate expected sequence from min to max
  expected_seq <- seq(min(ivals),
                      max(ivals),
                      by = gran)

  # Check all initialvalues are in the expected sequence (allowing for floating point errors)
  expect_true(all(sapply(ivals, function(x) any(abs(x - expected_seq) < 1e-8))))

})

