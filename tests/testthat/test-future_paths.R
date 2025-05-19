# _____________________________________________ #
# Set params to be used across tests ~~~ ####
# _____________________________________________ #

indicator      = "EG.ELC.ACCS.ZS"
pctlseq        = seq(20,80,20)
speedseq       = c(0.25,0.5,1,2,4)
startyear_data = 2000
start_year     = startyear_evaluation = 2000
end_year       = endyear_evaluation   = 2022
targetyear     = target_year = 2030
best           = "high"
floor          = 0
ceiling        = 100
granularity    = 0.1
lambdas        = 0.1*1.148^(0:50)

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
# Predictions ~~~ ####
# _____________________________________________ #

## Speed ####

fit_speed <- gcrq(change ~ ps(initialvalue,lambda=lambdas), foldid=data_model$fold_id, tau=0.5, data=data_model)

# Create dataset with the expected changes as a function of initial level.
predictions_speed <- as.data.frame(charts(fit_speed, k=seq(min,max,granularity))) |>
  mutate(initialvalue = round(seq(min,max,granularity)/granularity)*granularity) |>
  rename("change" = "V1") |>
  rowwise() |>
  # Expected changes can never give an outcome lower than the floor
  mutate(change = if_else(!is.na(floor),max(change,floor-initialvalue),change),
         # Expected changes can never give an outcome higher than the ceiling
         change = if_else(!is.na(ceiling),min(change,ceiling-initialvalue),change)) |>
  ungroup()

## Percentiles ####

fit_pctl <- gcrq(change ~ ps(initialvalue,lambda=lambdas), foldid=data_model$fold_id, tau=pctlseq/100, data=data_model)

# Create data set with the expected changes as a function of initial level.
predictions_pctl <- as.data.frame(charts(fit_pctl, k=seq(min,max,granularity))) |>
  mutate(initialvalue = round(seq(min,max,granularity)/granularity)*granularity) |>
  tidyr::pivot_longer(-initialvalue,names_to="pctl",values_to="change") |>
  mutate(pctl = 100*as.numeric(pctl)) |>
  rowwise() |>
  # Expected changes can never give an outcome lower than the floor
  mutate(change = if_else(!is.na(floor),max(change,floor-initialvalue),change),
         # Expected changes can never give an outcome higher than the ceiling
         change = if_else(!is.na(ceiling),min(change,ceiling-initialvalue),change)) |>
  ungroup() |>
  as.data.table()



# _____________________________________________ #
# TEST | Prep future data  ####
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

# -------------------------------------- #
# TEST | Future path -Percentiles  ####
# -------------------------------------- #

### Expected ------ ####

# Create a new dataset which will eventually contain the predicted path from the last observation to targetyear at all selected percentiles
path_fut_pctl <- expand.grid(code=unique(data_fut$code),year=seq(min(data_fut$year),targetyear,1),pctl=pctlseq) |>
  # Merge in the actual data from WDI
  joyn::joyn(data_fut,by=c("code","year"),match_type="m:1",keep="left",reportvar=FALSE)

# Year-by-year, calculate the percentile paths for the last value observed. For future target creation.
print("Calculating future percentile paths")
startyear_target = min(path_fut_pctl$year)
n = startyear_target - min(path_fut_pctl$year) + 1
# Continue this iteratively until the target year
while (n+min(path_fut_pctl$year)-1 <=targetyear) {
  # Year processed concurrently
  print(n+min(path_fut_pctl$year)-1)
  path_fut_pctl <- path_fut_pctl |>
    # Merge in data with predicted changes based on initial levels
    joyn::joyn(predictions_pctl,match_type="m:1",keep="left",by=c("y_fut=initialvalue","pctl"),reportvar=FALSE, verbose=FALSE) |>
    group_by(code,pctl) |>
    arrange(year) |>
    # Calculate new level based on the predicted changes.
    mutate(y_fut = if_else(row_number()==n & !is.na(lag(y_fut)),round((lag(y_fut)+lag(change))/granularity)*granularity,y_fut)) |>
    ungroup() |>
    select(-change)
  n=n+1
}
path_fut_pctl <- path_fut_pctl |>
  filter(!is.na(y_fut)) |>
  filter(between(y,min,max) | is.na(y)) |> as.data.table()


test_that("future_path_pctls() works as expected", {

  # Run your function
  my_path_fut_pctl <- future_path_pctls(
    data_fut = prep_data_fut(),
    predictions_pctl = predictions_pctl
  )

  # Sort both for consistent comparison (order-independent)
  data.table::setkeyv(my_path_fut_pctl,
                      c("code", "year", "pctl"))
  data.table::setkeyv(path_fut_pctl,
                      c("code", "year", "pctl"))

  # Compare entire tables
  expect_equal(my_path_fut_pctl,
               path_fut_pctl,
               ignore_attr = TRUE)

  # Basic structure checks
  expect_true(
    all(
      c("code", "year", "pctl", "y_fut") %in% names(my_path_fut_pctl)
      ))

  expect_true(
    is.numeric(my_path_fut_pctl$y_fut)
    )

  # NA check in key years (e.g., target year)
  expect_false(any(is.na(my_path_fut_pctl[year == targetyear]$y_fut)))


})


# -------------------------------------- #
# TEST | Future path -Speed ####
# -------------------------------------- #

### Expected ------ ####

predictions_speed <- predict_speed(data_model = data_model)
path_speed <- get_speed_path(predictions_speed = predictions_speed)



# Creates dataset with the country-years-speeds where projections are needed
data_fut <- expand.grid(code=unique(data_fut$code),year=seq(min(data_fut$year),targetyear,1),speed=speedseq) |>
  rename("yeartemp" = "year") |>
  joyn::joyn(data_fut,match_type="m:1",by="code",reportvar=FALSE,verbose=FALSE,y_vars_to_keep="year") |>
  filter(yeartemp>=year) |>
  select(-year) |>
  rename("year" = "yeartemp") |>
  joyn::joyn(data_fut,match_type="m:1",by=c("code","year"),reportvar=FALSE)

# Create a new dataset which will eventually contain the predicted path from the last observation to targetyear at all selected speeds
path_fut_speed <- data_fut |>
  select(-y) |>
  filter(!is.na(y_fut)) |>
  cross_join(path_speed) |>
  mutate(bst = best) |>
  filter(if_else(bst=="high",y_fut<=y,y_fut>=y)) |>
  group_by(code,speed) |>
  arrange(time) |>
  mutate(year = year + (time-time[1])/speed) |>
  ungroup() |>
  select(-c(y_fut,time,bst)) |>
  rename("y_fut" = "y") |>
  joyn::joyn(data_fut,match_type="1:1",by=c("code","year","speed"),reportvar=FALSE,verbose=FALSE,y_vars_to_keep="y") |>
  group_by(code,speed) |>
  arrange(year) |>
  mutate(y_fut = zoo::na.approx(y_fut,year,na.rm=FALSE,rule=2)) |>
  filter(year %in% seq(min(year),targetyear,1)) |>
  ungroup() |>
  filter(!is.na(y_fut)) |>
  # Only keep cases where target has not been reached
  filter(between(y,min,max) | is.na(y))


### Expected ------ ####

test_that("future_path_speed() works as expected", {

  # Run your function
  my_path_fut_speed <- future_path_speed(
    data_fut = prep_data_fut(),
    path_speed = path_speed
  )

  # Ensure both are data.tables and sorted the same way
  setkeyv(my_path_fut_speed, c("code", "year", "speed"))
  path_fut_speed <- as.data.table(path_fut_speed)
  setkeyv(path_fut_speed, c("code", "year", "speed"))

  # Main comparison
  expect_equal(my_path_fut_speed, path_fut_speed)

  # Check expected columns
  expect_true(all(c("code", "year", "speed", "y_fut", "y") %in% names(my_path_fut_speed)))

  # Check no missing y_fut values
  expect_false(any(is.na(my_path_fut_speed$y_fut)))

  # Check year is within range
  expect_true(all(my_path_fut_speed$year >= min(data_fut$year)))
  expect_true(all(my_path_fut_speed$year <= targetyear))


})

