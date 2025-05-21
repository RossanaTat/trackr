# Wrapper to run whole method #########

# data_wdi <- wbstats::wb_data(indicator = indicator,lang = "en",country="countries_only")
# indicator      = "EG.ELC.ACCS.ZS"



run_method <- function(data,
                       indicator      = NULL,
                       speed          = FALSE,
                       percentiles    = TRUE,
                       startyear_data = 2000,
                       start_year     = 2000,
                       end_year       = 2022,
                       target_year    = 2030,
                       floor          = 0,
                       ceiling        = 100,
                       granularity    = 0.1,
                       code_col       = "iso3c",
                       year_col       = "date",
                       pctlseq        = seq(20,80,20),
                       min            = NULL,
                       max            = NULL,
                       lambdas        = 0.1*1.148^(0:50),
                       best           = "high",
                       future         = FALSE,
                       verbose        = TRUE) {

  # # Input Validation & Checks #

  required_cols <- c(indicator,
                     code_col,
                     year_col)

  missing_cols <- setdiff(required_cols,
                          names(data))

  if (length(missing_cols) > 0) {
    cli::cli_abort("The following required columns are missing in `data`: ",
         paste(missing_cols, collapse = ", "))
  }

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

  # Retrieve min and max from data model
  if (is.null(min)) {
    min <- data_model$min
  }

  if (is.null(max)) {
    max <- data_model$max
  }

  print(min)
  print(max)



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

  future_path_out <- NULL

  if (future == TRUE) {

    data_fut <- prep_data_fut(
      data        = data,
      indicator   = indicator,
      granularity = granularity,
      code_col    = code_col,
      year_col    = year_col,
      verbose     = verbose
    )

    future_path <- future_path(
      data_fut         = data_fut,
      target_year      = target_year,
      min              = min,
      max              = max,
      granularity      = granularity,
      pctlseq          = pctlseq,
      predictions_pctl = predicted_changes$predictions_pctls,
      speedseq         = speedseq,
      path_speed       = predicted_changes$path_speed,
      best             = best,
      verbose          = verbose,
      speed            = speed,
      percentiles      = percentiles)

  }


  # ___________________________ #
  # 5. Scores ####
  # ___________________________ #

  scores <- get_scores(
    speed          = speed,
    pctl           = percentiles,
    path_his_pctl  = path_historical$percentile_path,
    best           = best,
    path_his_speed = path_historical$speed_path,
    path_speed     = predicted_changes$path_speed,
    min            = min,
    max            = max,
    granularity    = granularity,
    verbose        = verbose
  )


  if (verbose) {
    cli::cli_alert_info(
      cli::col_blue("✔ Method run completed.\n• Scores calculated\n• Historical and predicted paths generated\n• Output ready for use")
    )
  }


  return(invisible(list(
    data_model        = data_model,
    predicted_changes = predicted_changes,
    data_historical   = data_his,
    path_historical   = path_historical,
    future_path       = future_path_out,
    scores            = scores
  )))





}
