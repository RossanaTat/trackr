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

# --------------------------- . --------------------------- ####


test_that("prep data function works as expected", {

  prep_data <- prep_data()



  #### ADD TESTS HERE TO COMPARE WITH DATA MODEL ############



})


