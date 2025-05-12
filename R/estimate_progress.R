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
    min = round(if_else(is.na(floor),
                        min(data_model$initialvalue),
                        floor)/granularity)*granularity

  }

  if (is.null(max)) {
    max = round(if_else(is.na(ceiling),
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
