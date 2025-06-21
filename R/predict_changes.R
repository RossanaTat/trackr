# Functions to predict changes in indicator

#' Predict Speed of Change Based on Initial Value
#'
#' Fits a generalized constrained regression quantile model (`gcrq`) to estimate
#' the change in a variable as a function of its initial value
#' The function can restrict predictions to a range between a specified `floor` and `ceiling`
#' (e.g., targets), or default to the observed data range if these are not specified.
#'
#' @param data_model A data frame with the training data. Obtained through `prep_data()`. Must contain:
#'   - `initialvalue`: numeric, the initial level of the indicator.
#'   - `change`: numeric, the change in the indicator.
#'   - `fold_id`: numeric or factor, used for cross-validation.
#' @param min Optional. Minimum value of `initialvalue` to predict. If `NULL`,
#'   it is set to `floor` (if provided) or `min(data_model$initialvalue)`.
#' @param max Optional. Maximum value of `initialvalue` to predict. If `NULL`,
#'   it is set to `ceiling` (if provided) or `max(data_model$initialvalue)`.
#' @param lambdas Optional. A vector of lambda values, i.e., levels of flexibility that should be tried and evaluated through cross validation
#'   Defaults to `0.1 * 1.148^(0:50)`.
#' @param granularity Numeric. Granularity in outcome variable. Default is `0.1`.
#' @param floor Numeric or `NULL`.Minimum value of indicator.
#'   If `NULL`, predictions are unrestricted on the lower end.
#' @param ceiling Maximum value of indicator (NA if none).
#'   If `NULL`, predictions are unrestricted on the upper end.
#' @param verbose. Logical. If TRUE, display messages in console. Default is TRUE
#'
#' @return A data frame with:
#'   - `initialvalue`: sequence of values used for prediction.
#'   - `predictions_speed`: predicted change.
#'
predict_speed <- function(data_model,
                          min         = NULL,
                          max         = NULL,
                          lambdas     = NULL,
                          granularity = 0.1,
                          floor       = 0,
                          ceiling     = 100,
                          verbose     = TRUE) {

  # Ensure data_model is a data.table
  data_model <- as.data.table(data_model)

  # Early return if empty
  if (nrow(data_model) == 0) {
    cli::cli_alert_warning("Input data_model is empty. Returning NA data.table.")
    return(data.table(initialvalue = NA_real_,
                      change = NA_real_))
  }

  # Set min and max
  if (is.null(min)) {
    min <- round(if (is.null(floor)) min(data_model$initialvalue) else floor / granularity) * granularity
  }

  if (is.null(max)) {
    max <- round(if (is.null(ceiling)) max(data_model$initialvalue) else ceiling / granularity) * granularity
  }

  # Set lambdas
  if (is.null(lambdas)) {
    lambdas <<- 0.1 * 1.148^(0:50)
  }

  # Fit model
  fit_speed <- gcrq(change ~ ps(initialvalue, lambda = 0.1 * 1.148^(0:50)),
                    foldid = data_model$fold_id,
                    tau    = 0.5,
                    data   = data_model)

  # Generate prediction sequence
  x_seq <- seq(min, max, granularity)

  # Predict (charts() returns a named vector or data.frame)
  predictions_speed <- charts(fit_speed, k = x_seq)

  # Convert to data.table and compute columns
  predictions_dt <- data.table(change = as.numeric(predictions_speed))
  predictions_dt[, initialvalue := round(x_seq / granularity) * granularity]

  # Apply floor and ceiling constraints
  predictions_dt[, change := pmax(change, floor - initialvalue)]
  predictions_dt[, change := pmin(change, ceiling - initialvalue)]

  #setnames(predictions_dt, "change", "predictions_speed")

  if (verbose) cli::cli_alert_success("Predictions speed successfully calculated")

  return(predictions_dt)
}


#' Generate Speed Path from Predicted Changes
#'
#' Computes the expected trajectory (or "speed path") of a variable over time,
#' based on predicted annualized changes as a function of the variable's initial value.
#' This is typically used to simulate progress from the worst to the best value
#' of an indicator
#'
#'
#' @param predictions_speed A data.table containing two columns:
#'   \code{initialvalue} (the starting level of the indicator) and
#'   \code{change} (the expected annualized change from that level).
#' @param granularity A numeric value indicating the step size used when
#'   computing the trajectory over time. Smaller values produce smoother curves.
#'   Defaults to \code{0.1}.
#' @param best A character string indicating the direction of improvement.
#'   Use \code{"high"} (default) if higher values of the indicator are better,
#'   or \code{"low"} if lower values are better. This affects the direction of the path.
#' @param verbose Logical. If \code{TRUE} (default), a success message is displayed after computation.
#'
#' @return A \code{data.table} with two columns:
#'   \item{time}{Cumulative time (in years) needed to reach each step in the path.}
#'   \item{y}{The corresponding value of the indicator at each time step.}
#'
#'
#' get_speed_path(predictions_speed, granularity = 1, best = "high")
#'
#' @export
get_speed_path <- function(predictions_speed,
                           granularity = 0.1,
                           best        = "high",
                           verbose     = TRUE) {

  # Add validations on inputs ####

  if (best=="low") {

    predictions_speed <- predictions_speed |>
      roworder(-initialvalue) |>
      fmutate(change=-change)

  }

  path_speed <- predictions_speed |>
    ftransform(
      y    = initialvalue,
      time = change
    ) |>
    ftransform(
      time = {
        ltime        <- L(time)  # Lagged time
        time_new     <- 1 / ltime * granularity
        time_new[1L] <- 0  # Set first to 0
        cumsum(time_new)
      }
    ) |>
    fsubset(!is.infinite(time) & !is.nan(time)) |>
    fselect(time,
            y) |>
    as.data.table()

  if (verbose) cli::cli_alert_success("Path speed successfully calculated")

  return(path_speed)

}

#' Predict percentiles
#'
#' This function calculates the changes as a function of initial values by percentile
#'
#' @inheritParams predict_speed
#' @param verbose Logical. If TRUE, prints info messages in console
#' @param sequence_pctl Percentiles to calculate. The percentile score assigned is determined by the granularity of the percentiles selected here. I.e., if seq(20,80,20) is selected, then we will be able to tell if a country is between 0-20, 20-40, etc.
#' @return A data frame with calculated changes by percentiles.
#' @export
predict_pctls <- function(data_model,
                          min           = NULL,
                          max           = NULL,
                          granularity   = 0.1,
                          sequence_pctl = seq(20,80,20),
                          floor         = 0,
                          ceiling       = 100,
                          verbose       = TRUE,
                          lambdas       = NULL) {

  # __________________________________ #
  # Validate input ~~~~~ ####
  # __________________________________ #


  if (is.null(min)) {
    min = round(if_else(is.null(floor),
                        min(data_model$initialvalue),
                        floor)/granularity)*granularity

  }

  if (is.null(max)) {
    max = round(if_else(is.null(ceiling),
                        max(data_model$initialvalue),
                        ceiling)/granularity)*granularity

  }

  # Uses cross-validation to find the optimal smoothing of percentile-curves.
  # gcrq automatically ensures that the percentile-curves do not cross


  if (is.null(lambdas)) {
    lambdas <<- 0.1 * 1.148^(0:50)
  }

  # Inject lambdas into the data_model so gcrq can find it

  # Use formula with lambda as a name
  fit_pctl <- gcrq(change ~ ps(initialvalue, lambda = 0.1 * 1.148^(0:50)),
                   foldid = data_model$fold_id,
                   tau = sequence_pctl / 100,
                   data = data_model)


  predictions_pctl <- as.data.frame(charts(fit_pctl,
                                           k = seq(min, max, granularity))) |>

    mutate(initialvalue = round(seq(min,max,granularity)/granularity)*granularity) |>
    tidyr::pivot_longer(-initialvalue,
                        names_to="pctl",
                        values_to="change") |>
    mutate(pctl = 100*as.numeric(pctl)) |>
    rowwise() |>
    # Expected changes can never give an outcome lower than the floor
    mutate(change = if_else(!is.na(floor),
                            max(change, floor-initialvalue), change),
           # Expected changes can never give an outcome higher than the ceiling
           change = if_else(!is.na(ceiling), min(change,
                                                 ceiling-initialvalue),
                            change)) |>
    ungroup() |>
    as.data.table()


  if (verbose) {cli::cli_alert_success(
    "Changes as function of initial vals by percentiles successfully calculated"
  )}
  return(predictions_pctl)


}



#' Predict Changes in an indicator Over Time
#'
#' This function calculates projected changes in an indicator based on its initial value.
#' Two methods are available: `"speed"` and `"percentiles"`.
#'
#' @param data A data frame or list representing the input data model to be used for the prediction.
#' This should be the output of the [`prep_data()`] function, which prepares the data in the required format.
#' @param speed Logical. If TRUE, calculate speed of progress scores.
#' Default is FALSE
#' @param percentiles Logical. If TRUE, calculate percentile scores. Default is TRUE
#' @inheritParams predict_speed
#' @inheritParams predict_pctls
#' @inheritParams get_speed_path
#' @return A list containing prediction results. The content depends on the selected method:
#' \describe{
#'   \item{`predictions_speed`}{Predicted changes using the `"speed"` method (if selected).}
#'   \item{`path_speed`}{The path or trajectory of predictions based on the `"speed"` method (if selected).}
#'   \item{`predictions_pctls`}{Predicted changes using the `"percentiles"` method (if selected).}
#' }
#'
#' @details
#' - On The `"speed"` method: for indicators without overall progress, the speed of progress cannot be computed.
#' - The `"percentiles"` method can be computed for all indicators but ideally require rich training data.
#'
#' @seealso [predict_speed()], [get_speed_path()], [predict_pctls()]
#'
#' @export
predict_changes <- function(data,
                            min           = NULL,
                            max           = NULL,
                            floor         = 0,
                            ceiling       = 100,
                            granularity   = 0.1,
                            lambdas       = NULL,
                            best          = "high",
                            speed         = FALSE,
                            percentiles   = TRUE,
                            sequence_pctl = seq(20,80,20),
                            verbose       = TRUE) {

  # Add validations on inputs

  # at least one between speed and percentiles should be true
  if(speed == FALSE && percentiles == FALSE) {
    cli::cli_abort("At least one between speed and percentiles should be TRUE")
  }

  if(percentiles == TRUE & is.null(pctlseq)) {
    cli::cli_abort("If percentiles method is selected, need to provide pctls sequence")
  }

  # Validate 'best'
  if (!best %in% c("high", "low")) {
    cli::cli_abort("`best` must be either 'high' or 'low'")
  }

  if (verbose) {cli::cli_alert_info("Selected speed method for computations: {.strong {speed}}")}
  if (verbose) {cli::cli_alert_info("Selected pctls method for computations: {.strong {percentiles}}")}


  res_list <- list()

  if (speed) {

    # Predict speed
    predictions_speed <- predict_speed(data_model  = data,
                                       min         = min,
                                       max         = max,
                                       lambdas     = lambdas,
                                       granularity = granularity,
                                       floor       = floor,
                                       ceiling     = ceiling,
                                       verbose     = verbose)

    # Get speed paths
    path_speed <- get_speed_path(predictions_speed = predictions_speed,
                                 granularity       = granularity,
                                 best              = best,
                                 verbose           = verbose)

    res_list$changes_speed <- predictions_speed
    res_list$path_speed        <- path_speed

  }

  if (percentiles) {

    changes_pctl <- predict_pctls(data_model    = data,
                                       min           = min,
                                       max           = max,
                                       granularity   = granularity,
                                       floor         = floor,
                                       ceiling       = ceiling,
                                       sequence_pctl = sequence_pctl,
                                       verbose       = verbose)

    res_list$changes_pctl <- changes_pctl
  }

  return(
    invisible(res_list)
  )

}

