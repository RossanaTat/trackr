# Wrapper to run whole method #########

# data_wdi <- wbstats::wb_data(indicator = indicator,lang = "en",country="countries_only")
# indicator      = "EG.ELC.ACCS.ZS"


#' Track Progress on an Indicator Over Time
#'
#' Calculates progress scores and future targets for a WDI indicator of choice, depending on user selection of speed, percentiles, and future projections.
#'
#' @inheritParams prep_data
#' @inheritParams predict_changes
#' @inheritParams future_path
#' @inheritParams get_scores
#'
#' @return An (invisible) list containing:
#' \describe{
#'   \item{data_model}{Cleaned and normalized indicator data.}
#'   \item{predicted_changes}{Output from `predict_changes()`.}
#'   \item{data_historical}{Subset of original data used for historical paths.}
#'   \item{path_historical}{Historical percentile and speed paths.}
#'   \item{future_path}{(Optional) Projected future paths; `NULL` if `future = FALSE`.}
#'   \item{scores}{Performance scores based on historical and projected trends.}
#' }
#'
#' @seealso [prep_data()], [predict_changes()], [future_path()], [get_scores()]
#' @export
track_progress <- function(indicator      = NULL,
                           data           = wbstats::wb_data(indicator = indicator,
                                                   lang = "en", country = "countries_only"),
                           code_col       = "iso3c",
                           year_col       = "date",
                           speed          = FALSE,
                           percentiles    = TRUE,
                           startyear_data = 2000,
                           eval_from      = 2000,
                           eval_to        = 2022,
                           target_year    = 2030,
                           floor          = 0,
                           ceiling        = 100,
                           granularity    = 0.1,
                           sequence_pctl  = seq(20,80,20),
                           speedseq       = c(0.25, 0.5, 1, 2, 4),
                           min            = NULL,
                           max            = NULL,
                           lambdas        = 0.1*1.148^(0:50),
                           best           = NULL,
                           future         = FALSE,
                           verbose        = TRUE) {

  # # Input Validation & Checks #
  # To add here ##

  required_cols <- c(indicator,
                     code_col,
                     year_col)

  missing_cols <- setdiff(required_cols,
                          names(data))

  if (length(missing_cols) > 0) {
    cli::cli_abort("The following required columns are missing in `data`: ",
         paste(missing_cols, collapse = ", "))
  }

  # Validate `best` argument
  if (is.null(best)) {

    cli::cli_abort("`best` must be provided: either 'high' or 'low'")
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
                                       sequence_pctl  = sequence_pctl,
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
    eval_from   = eval_from,
    eval_to    = eval_to,
    granularity = granularity
  )

  path_historical <- path_historical(
    percentiles      = percentiles,
    speed            = speed,
    data_his         = data_his,
    eval_from        = eval_from,
    eval_to          = eval_to,
    granularity      = granularity,
    floor            = floor,
    ceiling          = ceiling,
    min              = min,
    max              = max,
    sequence_pctl    = sequence_pctl,
    changes_pctl     = predicted_changes$changes_pctl,
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

    future_path_out <- future_path(
      data_fut         = data_fut,
      target_year      = target_year,
      min              = min,
      max              = max,
      granularity      = granularity,
      sequence_pctl    = sequence_pctl,
      changes_pctl     = predicted_changes$changes_pctl,
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
    path_his_pctl  = path_historical$pctl,
    best           = best,
    path_his_speed = path_historical$speed,
    path_speed     = predicted_changes$path_speed,
    min            = min,
    max            = max,
    granularity    = granularity,
    verbose        = verbose
  )


  # if (verbose) {
  #   cli::cli_alert_success(
  #     cli::col_blue("✔ Method run completed.\n• Scores calculated\n• Historical and predicted paths generated\n• Output ready for use")
  #   )
  # }

  if (verbose) {
    components <- c()

    if (percentiles) {
      components <- c(components, "percentile scores")
    }

    if (speed) {
      components <- c(components, "speed scores")
    }

    if (future) {
      components <- c(components, "future path")
    }

    # Always include these
    components <- c("data model", "historical paths", "predicted changes", components)

    cli::cli_alert_success(
      cli::col_blue(
        paste0("Method run completed for indicator: '", indicator, "'.\n• Output includes: ",
               paste(components, collapse = ", "), ".")
      )


    )
  } ### Messages completed


  return(invisible(list(
    data_model        = data_model,
    predicted_changes = predicted_changes,
    data_historical   = data_his,
    path_historical   = path_historical,
    path_future       = future_path_out,
    scores            = scores
  )))





}
