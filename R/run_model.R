# Wrapper to run whole method #########

# data_wdi <- wbstats::wb_data(indicator = indicator,lang = "en",country="countries_only")
# indicator      = "EG.ELC.ACCS.ZS"
# indicator = "NY.GDP.PCAP.KD"


#' Track Progress on an Indicator Over Time
#'
#' Evaluates countries’ progress in an indicator over a specified time period.
#' Progress scores will either be calculated using a ‘speed of progress’ metric and/or by a ‘percentile score’ metric.
#' The former evaluates countries by how fast they developed relative to the typical experience, expressed in years. A speed of progress of 2, for example, means that a country developed twice as fast as the typical experience.
#' Percentile scores evaluate countries progress by the share of experiences that they are outperforming.
#' A score of 90, for example, means that a country outperformed 90% of experiences observed. Alternatively (or additionally) Calculates progress scores and future targets for an indicator of choice, depending on user selections
#'
#' @inheritParams prep_data
#' @inheritParams predict_changes
#' @inheritParams path_historical
#' @inheritParams future_path
#' @inheritParams get_scores
#' @param future Logical. If TRUE, projections to the future will be made using the speeds of sequence_speed (if speed=TRUE) and the percentile paths of sequence_pctl (if `percentiles = TRUE`).
#' @return An (invisible) list containing:
#' \describe{
#'   \item{data_model}{The indicator data used to fit the model.}
#'   \item{predicted_changes}{Median changes in the indicator as a function of the level of the indicator (`changes_speed`), or quantile specific changes in the indicator as a function of the level of the indicator(`changes_pctl`), or the typical path from the worst outcome in the indicator to the best outcome in the indicator (path_speed) }
#'   \item{path_historical}{The historical data as well as predicted paths from each country’s starting value at various speeds and/or percentile paths.}
#'   \item{future_path}{(Optional) Projected future paths from the last observation for each country at various speeds and/or percentile paths; `NULL` if `future = FALSE`.}
#'   \item{scores}{Progress scores for the evaluation period selected.}
#' }
#'
#' @seealso [prep_data()], [predict_changes()], [future_path()], [get_scores()]
#' @export
track_progress <- function(data           = NULL,
                           indicator      = NULL,
                           code_col       = "iso3c",
                           year_col       = "date",
                           startyear_data = NULL,
                           endyear_data   = NULL,
                           eval_from      = NULL,
                           eval_to        = NULL,
                           speed          = FALSE,
                           percentiles    = TRUE,
                           future         = FALSE,
                           target_year    = 2030,
                           sequence_pctl  = seq(20,80,20),
                           sequence_speed = c(0.25, 0.5, 1, 2, 4),
                           best           = NULL,
                           min            = NULL,
                           max            = NULL,
                           support        = 1,
                           granularity    = 0.1,
                           verbose        = TRUE) {

  # ___________________________ #
  # Validation of Inputs ####
  # ___________________________ #

  required_cols <- c(indicator,
                     code_col,
                     year_col)

  missing_cols <- setdiff(required_cols,
                          names(data))

  if (length(missing_cols) > 0) {

    cli::cli_abort("The following required columns are missing in `data`: ",
         paste(missing_cols,
               collapse = ", "))
  }

  # Validate `best` argument
  if (is.null(best)) {

    cli::cli_abort("`best` must be provided: either 'high' or 'low'")
  }

  # Validate data and indicator params

  if (is.null(indicator) && is.null(data)) {

    cli::cli_abort("User must provide both indicator name and input data")

  }
  # ___________________________ #
  # 1. Prepare Data ####
  # ___________________________ #

  data_model <- prep_data(indicator      = indicator,
                          data           = data,
                          startyear_data = startyear_data,
                          endyear_data   = endyear_data,
                          support        = support,
                          granularity    = granularity,
                          code_col       = code_col,
                          year_col       = year_col,
                          min            = min,
                          max            = max,
                          verbose        = verbose
                          )

  # Retrieve min and max from data model
  min <- data_model$min
  max <- data_model$max

  # ___________________________ #
  # 2. Predict Changes ####
  # ___________________________ #

  predicted_changes <- predict_changes(data           = data_model$data_model,
                                       min            = min,
                                       max            = max,
                                       granularity    = granularity,
                                       best           = best,
                                       speed          = speed,
                                       percentiles    = percentiles,
                                       sequence_pctl  = sequence_pctl,
                                       #support        = support,
                                       verbose        = verbose)


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
    eval_to     = eval_to,
    granularity = granularity
  )

  path_historical <- path_historical(
    percentiles      = percentiles,
    speed            = speed,
    data_his         = data_his,
    eval_from        = eval_from,
    eval_to          = eval_to,
    granularity      = granularity,
    min              = min,
    max              = max,
    sequence_pctl    = sequence_pctl,
    changes_pctl     = predicted_changes$changes_pctl,
    verbose          = verbose,
    sequence_speed   = sequence_speed,
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
      sequence_speed   = sequence_speed,
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
      cli::col_cyan(
        paste0("Method run completed for indicator: '", indicator, "'.\n• Output includes: ",
               paste(components, collapse = ", "), ".")
      )


    )
  } ### Messages completed


  return(invisible(list(
    data_model        = data_model,
    predicted_changes = predicted_changes,
    path_historical   = path_historical,
    path_future       = future_path_out,
    scores            = scores
  )))

}
