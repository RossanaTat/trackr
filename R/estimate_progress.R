# Functions to predict changes in indicator

#' Predict Speed of Change Based on Initial Value
#'
#' Fits a generalized constrained regression quantile model (`gcrq`) to estimate
#' the change in a variable as a function of its initial value
#' The function can restrict predictions to a range between a specified `floor` and `ceiling`
#' (e.g., targets), or default to the observed data range if these are not specified.
#'
#' @param data_model A data frame with the training data. Must contain:
#'   - `initialvalue`: numeric, the initial level of the indicator.
#'   - `change`: numeric, the change in the indicator.
#'   - `fold_id`: numeric or factor, used for cross-validation.
#' @param min Optional. Minimum value of `initialvalue` to predict. If `NULL`,
#'   it is set to `floor` (if provided) or `min(data_model$initialvalue)`.
#' @param max Optional. Maximum value of `initialvalue` to predict. If `NULL`,
#'   it is set to `ceiling` (if provided) or `max(data_model$initialvalue)`.
#' @param lambdas Optional. A vector of lambda values used in the penalized spline.
#'   Defaults to `0.1 * 1.148^(0:50)`.
#' @param granularity Numeric. Step size for the prediction sequence. Default is `0.1`.
#' @param floor Numeric or `NULL`. Lower bound for the final level (`initialvalue + change`).
#'   If `NULL`, predictions are unrestricted on the lower end.
#' @param ceiling Numeric or `NULL`. Upper bound for the final level.
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
                          ceiling     = 100) {

  # Validation of inputs ####

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

  if (is.null(lambdas)) {
    lambdas <- 0.1*1.148^(0:50)
  }


  # TO COMPLETE

  # Fit the model (this line stays the same)
  fit_speed <- gcrq(change ~ ps(initialvalue, lambda = lambdas),
                    foldid = data_model$fold_id,
                    tau    = 0.5,
                    data   = data_model)

  # Generate prediction sequence
  x_seq <- seq(min, max, granularity)

  # Create prediction data
  predictions_speed <- charts(fit_speed, k = x_seq)

  # Convert to fdata.frame and assign rounded initial values
  predictions_speed <- fmutate(data.frame(predictions_speed),
                               initialvalue = round(x_seq / granularity) * granularity) |>
    frename("predictions_speed" = "change")

  predictions_speed <- fmutate(predictions_speed,
                               change = pmax(change, floor - initialvalue),
                               change = pmin(change, ceiling - initialvalue))

  return(predictions_speed)

}

#' Get speed paths
#'
#' Calculates the expected path over time from the worst value to the best value.
#'
#' @inheritParams predict_speed
#' @param best Character string, either \code{"high"} (default) or \code{"low"}, indicating
#'   whether higher or lower values of the initial variable are considered better.
#' @return data frame with speed path
#'
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
            y)

  if (verbose) cli::cli_alert_success("Path speed successfully calculated")

  return(path_speed)

}

#' Predict percentiles
#'
#' This function calculates the changes as a function of initial values by percentile
#'
#' @inheritParams predict_speed
#' @return A data frame with calculated changes by percentiles.
#' @export
predict_pctls <- function(data_model,
                          min         = NULL,
                          max         = NULL,
                          granularity = 0.1,
                          floor       = 0,
                          ceiling     = 100,
                          verbose     = TRUE) {

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

  fit_pctl <- gcrq(change ~ ps(initialvalue,
                               lambda = lambdas),
                   foldid = data_model$fold_id,
                   tau = pctlseq/100,
                   data = data_model)

  predictions_pctl <- as.data.frame(charts(fit_pctl, k = seq(min, max, granularity))) |>

    fmutate(initialvalue = round(
      seq(min ,max, granularity)/granularity
    )*granularity)


  predictions_pctl <- predictions_pctl |>
    collapse::pivot(ids = "initialvalue",
                    values = names(predictions_pctl |> fselect(-initialvalue)),
                    names = list(variable = "pctl",
                                 value = "change"),
                    how = "longer") |>
    fmutate(pctl = 100*as.numeric(pctl)) |>

    # TODO - MOVE TO COLLAPSE
    rowwise() |>
    # Expected changes can never give an outcome lower than the floor
    mutate(change = if_else(!is.na(floor),
                            max(change,
                                floor-initialvalue),
                            change),
           # Expected changes can never give an outcome higher than the ceiling
           change = if_else(!is.na(ceiling),
                            min(change,ceiling-initialvalue),
                            change)) |>
    ungroup()


  if (verbose) {cli::cli_alert_success(
    "Changes as function of initial vals by percentiles successfully calculated"
                                       )}
  return(predictions_pctl)


}

#' Predict Changes in a Variable Over Time
#'
#' This function calculates projected changes in a variable based on its initial value.
#' Two methods are available: `"speed"` and `"percentiles"`.
#'
#' @param data A data frame or list representing the input data model to be used for the prediction.
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
#' - The `"speed"` method models change using a parametric growth model based on lambda values.
#' - The `"percentiles"` method estimates expected values based on percentile changes.
#'
#' @seealso [predict_speed()], [get_speed_path()], [predict_pctls()]
#'
#' @export
predict_changes <- function(data,
                            min         = NULL,
                            max         = NULL,
                            floor       = 0,
                            ceiling     = 100,
                            granularity = 0.1,
                            lambdas     = NULL,
                            best        = "high",
                            method      = c("speed", "percentiles"),
                            verbose     = TRUE) {

  # Add validations on inputs
  method <- match.arg(method)

  # Validate 'best'
  if (!best %in% c("high", "low")) {
    cli::cli_abort("`best` must be either 'high' or 'low'")
  }

  if (verbose) {cli::cli_alert_info("Selected method for computations: {.strong {method}}")}

  if (method == "speed") {

    # Predict speed
    predictions_speed <- predict_speed(data_model  = data,
                                       min         = min,
                                       max         = max,
                                       lambdas     = lambdas,
                                       granularity = granularity,
                                       floor       = floor,
                                       ceiling     = ceiling)

    # Get speed paths
    path_speed <- get_speed_path(predictions_speed = predictions_speed,
                                 granularity       = granularity,
                                 best              = best,
                                 verbose           = verbose)

    return(list(
      predictions_speed = predictions_speed,
      path_speed        = path_speed
    ))

  } else {

    predictions_percentiles <- predict_pctls(data_model  = data,
                                             min         = min,
                                             max         = max,
                                             granularity = granularity,
                                             floor       = floor,
                                             ceiling     = ceiling,
                                             verbose     = verbose
                                             )

    return(list(
      predictions_pctls = predictions_pctls
    ))
  }

}

