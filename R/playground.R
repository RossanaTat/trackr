# Running the package functions with a few indicators as an example

# General Params ######
# ______________ ######

code_col       = "iso3c"
year_col       = "date"
granularity    = 0.1
verbose        = TRUE
speed          = TRUE
percentiles    = TRUE
future         = TRUE
sequence_pctl  = seq(20,80,20)
sequence_speed = c(0.25, 0.5, 1, 2, 4)
min = 0
support = 1
# max = 100

# _________________________________________ #
## GDP Per Capita ####
# _________________________________________ #


indicator = "NY.GDP.PCAP.KD"
# data_wdi <- wbstats::wb_data(indicator = indicator,
#                              lang = "en",
#                              country="countries_only")
startyear_data                                = 1960
endyear_data                                  = 2022
eval_from                                     = 2015
eval_to                                       = 2024
target_year                                   = 2030
best                                          = "high"
max                                           = 89884.08
min = 122.6789
#ceiling  <- quantile(data_wdi$y, 0.99, na.rm = T)
granularity                                   = 1


# _________________________________________ #
## Access to Electricity ####
# _________________________________________ #

indicator      = "EG.ELC.ACCS.ZS"


# _________________________________________ #
## Female Labor Force Particip. ####
# _________________________________________ #

indicator = "SL.TLF.ACTI.FE.ZS"




