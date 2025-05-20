# Wrapper to run whole method #########


run_method <- function(data,
                       indicator,
                       speed,
                       percentiles,
                       startyear_data = 2000,
                       start_year     = 2000,
                       end_year       = 2022,
                       target_year    = 2030,
                       floor          = 0,
                       ceiling        = 100,
                       granularity    = 0.1,
                       code_col       = "iso3c",
                       year_col       = "date",
                       pctlseq,
                       min,
                       max,
                       lambdas,
                       best,
                       verbose        = TRUE) {

  # Input Validation & Checks #

  # TODO

  # ___________________________ #
  # 1. Prepare Data ####
  # ___________________________ #

  data_model <- prep_data(indicator      = indicator,
                          data           = data,
                          startyear_data = startyear_data,
                          floor          = floor,
                          ceiling        = ceiling,
                          granularity    = granularity,
                          code_col       = code_col,
                          year_col       = year_col,
                          verbose        = verbose
                          )


  # ___________________________ #
  # 2. Predict Changes ####
  # ___________________________ #

  predicted_changes <- predict_changes(data        = data_model$data_model,
                                       min         = min,
                                       max         = max,
                                       floor       = floor,
                                       ceiling     = ceiling,
                                       granularity = granularity,
                                       lambdas     = lambdas,
                                       best        = best,
                                       speed       = speed,
                                       percentiles = percentiles,
                                       pctlseq     = pctlseq,
                                       verbose     = verbose)


  # ___________________________ #
  # 3. Historical Paths ####
  # ___________________________ #

  data_his <- get_his_data(
    indicator   = indicator,
    data        = data,
    code_col    = code_col,
    year_col    = year_col,
    min         = min,
    max         = max,
    start_year  = start_year,
    end_year    = end_year,
    granularity = granularity
  )

  path_historical <- path_historical(
    percentiles      = percentiles,
    speed            = speed,
    data_his         = data_his,
    start_year       = start_year,
    end_year         = end_year,
    granularity      = granularity,
    floor            = floor,
    ceiling          = ceiling,
    min              = min,
    max              = max,
    pctlseq          = pctlseq,
    predictions_pctl = predicted_changes$predictions_pctls,
    verbose          = verbose,
    speedseq         = speedseq,
    path_speed       = predicted_changes$path_speed,
    best             = best
  )





  # ___________________________ #
  # 4. Future Paths ####
  # ___________________________ #


  # ___________________________ #
  # 5. Scores ####
  # ___________________________ #





}
