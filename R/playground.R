# Running the package functions with a few indicators as an example

# General Params ######
# ______________ ######

code_col       = "iso3c"
year_col       = "date"
#granularity    = 0.1
verbose        = TRUE
speed          = TRUE
percentiles    = TRUE
future         = TRUE
sequence_pctl  = seq(20,80,20)
sequence_speed = c(0.25, 0.5, 1, 2, 4)
min = 0
support = 2
extreme_percentile = getOption("trackr.extreme_pctl")
# max = 100

# _________________________________________ #
## GDP Per Capita ####
# _________________________________________ #


indicator = "NY.GDP.PCAP.KD"
data <- wbstats::wb_data(indicator = indicator,
                             lang = "en",
                             country="countries_only")
startyear_data                                = 1960
endyear_data                                  = 2022
eval_from                                     = 2015
eval_to                                       = 2024
target_year                                   = 2030
best                                          = "high"
max                                           = 89884.08
min = 122.6789
#ceiling  <- quantile(data_wdi$y, 0.99, na.rm = T)
granularity                                   = 0.6
support = 3

# _________________________________________ #
## Access to Electricity ####
# _________________________________________ #

indicator      = "EG.ELC.ACCS.ZS"


# _________________________________________ #
## Female Labor Force Particip. ####
# _________________________________________ #

indicator = "SL.TLF.ACTI.FE.ZS"


# _________________________________________ #
## Poverty Rate  ####
# _________________________________________ #

indicator = "rate"

# Load data

load(file = "C:\\Users\\wb621604\\OneDrive - WBG\\Desktop\\PIP\\poverty-country.Rda")

# Run model

new_results <- track_progress(
  data = poverty_country,
  indicator = "rate",
  code_col = "code",
  year_col = "year",
  startyear_data = 1950,
  endyear_data = 2025,
  eval_from = 2015,
  eval_to=2025,
  future = TRUE,
  target_year = 2050,
  speed=TRUE,
  percentiles=FALSE,
  sequence_pctl = seq(20, 80, 20),
  sequence_speed = c(0.25,0.5,1,2,4),
  best="low",
  min=0,
  max=100,
  support=1,
  granularity=0.01,
)


# ________________________________________________ #
## Fossil Fuel Sub. per unit of GDP  ####
# ________________________________________________ #

# Load data
#  data_ch12 <- readxl::read_excel("C:\\Users\\wb621604\\OneDrive - WBG\\Desktop\\PIP\\fossil_fuel_subsidy_glow.xlsx")
# #
# # # Run package code
# results <- track_progress(
#   data=data_ch12,
#   indicator="y",
#   code_col="code",
#   year_col="year",
#   startyear_data=2015,
#   endyear_data=2025,
#   eval_from=2015,
#   eval_to=2025,
#   future=FALSE,
#   target_year=2030,
#   speed=TRUE,
#   percentiles=TRUE,
#   sequence_pctl = seq(20, 80, 20),
#   sequence_speed = c(0.25,0.5,1,2,4),
#   best="low",
#   min=0,
#   max=max(data_ch12$y),
#   support=1,
#   granularity=0.01,
# )
#
# # # Inspect results
# #
# # ### Plot predicted path
# #
# path <- results$predicted_changes$path_speed
# ggplot(path,aes(y=y,x=time)) + geom_line()
# min(path$y)
#
# # The path stops at 3.4. Below that, changes are not on expectation in the right direction. You can also see that here:
#
# expectedchanges <- results$predicted_changes$changes_speed
# ggplot(expectedchanges,aes(y=change,x=y)) + geom_line()

# Here the x-axis is the initial level, and the y-axis is the expected change.
# For the speed method to work, the expected change has to be below zero regardless of the initial level.

# Only for countries with values greater than 3.4 can the speed score be calculated


# WDI Indicators     ######
# ___________________ ######


# Ch 1 ####

indicator = "SI.POV.DDAY"
# data_ch1 <- wbstats::wb_data(indicator = indicator,
#                          lang      = "en",
#                          country   = "countries_only")
#
# results_ch1 <- track_progress(
#   data           = data_ch1,
#   indicator      = indicator,
#   code_col       = "iso3c",
#   year_col       = "date",
#   startyear_data = 1960,
#   endyear_data   = 2024,
#   eval_from      = 2015,
#   eval_to        = 2023,
#   future         = FALSE,
#   target_year    = 2030,
#   speed          = TRUE,
#   percentiles    = TRUE,
#   sequence_pctl  = seq(20, 80, 20),
#   sequence_speed = c(0.25, 0.5, 1, 2, 4),
#   best           = "low",
#   min            = 0,
#   max            = NULL,
#   support        = 2,
#   granularity    = 0.01
# )

# Ch 2 ####

# indicator = "SN.ITK.DEFC.ZS"
# data_ch2 <- wbstats::wb_data(indicator = indicator,
#                              lang      = "en",
#                              country   = "countries_only")
#
# results_ch2 <- track_progress(
#   data           = data_ch2,
#   indicator      = indicator,
#   code_col       = "iso3c",
#   year_col       = "date",
#   startyear_data = 1960,
#   endyear_data   = 2024,
#   eval_from      = 2015,
#   eval_to        = 2024,
#   future         = TRUE,
#   target_year    = 2030,
#   speed          = TRUE,
#   percentiles    = TRUE,
#   sequence_pctl  = seq(20, 80, 20),
#   sequence_speed = c(0.25, 0.5, 1, 2, 4),
#   best           = "low",
#   min            = 0,
#   max            = NULL,
#   support        = 2,
#   granularity    = 0.01
# )

