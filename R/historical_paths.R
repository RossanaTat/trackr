
#____________________________ #
### HISTORICAL PATHS ####
#____________________________ #

#' Get historical baseline data
#'
#' Extract and format historical indicator data for each country to establish a baseline (y_his)
#' used to evaluate progress. This function selects data within a given time range,
#' filters out leading missing values, and stores the first valid (non-missing) observation—
#' rounded to a specified granularity—as the baseline value (`y_his`).
#'
#' @inheritParams prep_data
#' @inheritParams predict_changes
#' @param eval_from First year to include when evaluating the progress of countries.
#' @param eval_to Last year to include when evaluating the progress of countries.
#' @param support Numeric. Reflects minimum number of countries that must have experienced a particular outcome value from startyear_data to endyear_data for a progress score to be calculated. This limits the calculation of scores at extreme outcome values at which there is less support to evaluate what typical progress looks like. Will be overruled by min and max if those are more restrictive. Defaults to 1.
#'
#' @return A `data.table` containing the following columns:
#'   \item{code}{Country code (standardized).}
#'   \item{year}{Year of observation.}
#'   \item{y}{Value of the indicator.}
#'   \item{y_his}{First non-missing historical value (rounded to the specified granularity).}
#'
#' @examples
#' \dontrun{
#'   his_data <- get_his_data(indicator = "EG.ELC.ACCS.ZS",
#'                            eval_from = 2000,
#'                            eval_to = 2022,
#'                            granularity = 0.1)
#' }
#'
#' @export
get_his_data <- function(indicator          = "EG.ELC.ACCS.ZS",
                         data               = NULL,
                         code_col           = "iso3c",
                         year_col           = "date",
                         min                = NULL,
                         max                = NULL,
                         eval_from          = 2000,
                         eval_to            = 2022,
                         support            = 1,
                         granularity        = 0.1,
                         extreme_percentile = getOption("trackr.extreme_pctl")) {

  if (is.null(min) || is.null(max)) {
    cli::cli_abort(message = "min and max should be provided")
  }

  stopifnot(is.numeric(eval_from),
            is.numeric(eval_to),
            eval_from <= eval_to,
            is.numeric(granularity),
            granularity > 0)

  # Convert to data.table and standardize column names
  dt <- qDT(data)
  data.table::setnames(dt,
                       old         = c(code_col, year_col, indicator),
                       new         = c("code", "year", "y"),
                       skip_absent = FALSE)

  # Filter years
  dt <- dt[between(year, eval_from, eval_to), .(code, year, y)]
  setorder(dt, code, year)

  # Add rounded y for support analysis
  dt[, y_rounded := round(y / granularity) * granularity]

  # Compute support table using all y values
  support_table <- dt[!is.na(y_rounded), .(n_countries = uniqueN(code)), by = y_rounded]


  # --------------------------------------
  # Tail cutoff logic: support asymmetric percentiles ####
  # --------------------------------------

  if (length(extreme_percentile) == 1) {
    lower_pct <- extreme_percentile
    upper_pct <- extreme_percentile
  } else if (length(extreme_percentile) == 2) {
    lower_pct <- extreme_percentile[1]
    upper_pct <- extreme_percentile[2]
  } else {
    cli::cli_abort("extreme_percentile must be a numeric vector of length 1 or 2.")
  }

  lower_cutoff <- quantile(dt$y_rounded, probs = lower_pct, na.rm = TRUE)
  upper_cutoff <- quantile(dt$y_rounded, probs = 1 - upper_pct, na.rm = TRUE)


  support_table[, region := fifelse(y_rounded < lower_cutoff, "low",
                                    fifelse(y_rounded > upper_cutoff, "high", "middle"))]

  support_table[, keep := region == "middle" | n_countries >= support]

  # Supported values
  supported_values <- support_table[keep == TRUE, y_rounded]

  # Apply support filter
  dt <- dt[y_rounded %in% supported_values]

  # Apply strict min/max filtering
  dt <- dt[y_rounded >= min & y_rounded <= max]

  # Remove leading NAs to define historical baseline
  dt[, cum_nm := cumsum(!is.na(y)), by = code]
  dt <- dt[cum_nm != 0]

  # Assign y_his: first non-NA rounded y per country
  dt[, y_his := fifelse(seq_len(.N) == 1, y_rounded, NA_real_), by = code]

  # Final cleanup
  dt[, c("cum_nm", "y_rounded") := NULL]

  # Assign attributes
  setattr(dt, "min", min)
  setattr(dt, "max", max)

  setorder(dt, code, year)

  if (support > 1) {
    cli::cli_alert_info("{length(supported_values)} rounded `y` levels retained after applying support >= {support} to tails and bounds [{min}, {max}].")
  }

  return(dt)
}

#' Historical speed path
#'
#' Calculates path a country would have taken with various speeds
#'
#' @inheritParams get_speed_path
#' @inheritParams project_pctls_path
#' @param sequence_speed Numeric vector of relative speeds of progress used for
#'   model-based future projections when `speed = TRUE`. A value of 1 represents
#'   the typical (median) speed observed historically; values above (below) 1
#'   represent faster (slower) progress.
#'
#'   In addition to these model-based speeds, in the case of future projections, these are always
#'   computed using each country’s historically estimated speed of progress
#'   (when available). Historical-speed projections are appended to the future
#'   speed paths and identified via a `speed_source` column.
#'
#'
#' @return A data table of projected values under different speeds
#'
#'
#' @export
project_path_speed <- function(data_his,
                               sequence_speed = c(0.25, 0.5, 1, 2, 4),
                               path_speed,
                               granularity    = 0.1,
                               eval_from      = 2000,
                               eval_to        = 2022,
                               min            = NULL,
                               max            = NULL,
                               best           = "high") {

  # Set min and max from attributes if not provided
  if (is.null(min)) min <- attr(data_his, "min")
  if (is.null(max)) max <- attr(data_his, "max")

  data_his_copy <- data_his[!is.na(y_his)]

  # # Cross join with sequence_speed
  speeds <- qDT(sequence_speed)[, k := 1]

  data_his_copy[, k := 1]

  data_his_copy <- joyn::merge(data_his_copy,
                          speeds,
                          by              = "k",
                          allow.cartesian = TRUE,
                          reportvar       = FALSE,
                          verbose         = FALSE)

  data_his[, k := 1]
  data_his <- joyn::merge(data_his,
                               speeds,
                               by              = "k",
                               allow.cartesian = TRUE,
                               reportvar       = FALSE,
                               verbose         = FALSE)
  data_his[, k := NULL]
  setnames(data_his, "sequence_speed", "speed")


  data_his_copy <- data_his_copy[, .(code, year, y_his, k, speed = sequence_speed, best)]

  # Cross join path speed

  path_speed_copy <- copy(path_speed)
  path_speed_copy[, k := 1]


  data_his_copy <- joyn::merge(data_his_copy,
                               path_speed_copy,
                               by              = "k",
                               allow.cartesian = TRUE,
                               reportvar       = FALSE,
                               verbose         = FALSE)

  if (best == "high") {
    data_his_copy <- data_his_copy[y_his <= y]
  } else if (best == "low") {
    data_his_copy <- data_his_copy[y_his >= y]

  }

  data_his_copy[, year := year + (time - time[1]) / speed,
                by = .(code, speed)]

  data_his_copy[, c("best", "y_his", "time") := NULL]

  setnames(data_his_copy,
           "y", "y_speed")

  data_his_copy <- data_his_copy |>
    joyn::joyn(data_his,
             match_type     = "1:1",
             by             = c("code","year","speed"),
             reportvar      = FALSE,
             y_vars_to_keep = "y",
             verbose        = FALSE)

  # keep original row order
  data_his_copy[, orig_order := .I]

  # replicate: group_by(code, speed) |> arrange(year) |> mutate(...)
  data_his_copy <- data_his_copy[
    order(code, speed, year),
    y_speed := zoo::na.approx(y_speed, x = year, na.rm = FALSE, rule = 2),
    by = .(code, speed)
  ]

  # restore original row order
  setorder(data_his_copy, orig_order)
  data_his_copy[, orig_order := NULL]   # drop helper column

  data_his_copy[year %in% seq(eval_from, eval_to)]

  # # Filter for actual values between min and max
  data_his_copy <- data_his_copy[y >= min, ][y <= max, ][, k := NULL]


  if (verbose) {
    if (is.null(data_his_copy)) {
      cli::cli_alert_danger("Computed path dt is empty")
    }
  }

  # Output _____ #
  ################

  # y_speed:  Simulated trajectory values from path_speed (non-regular years)
  # y      :  Actual observed data

  return(data_his_copy[])
}




#' Compute historical paths
#'
#' Wrapper to compute historical paths by percentiles and/or speed
#'
#' @inheritParams predict_changes
#' @inheritParams get_his_data
#' @inheritParams project_pctls_path
#' @param data_his A data.table containing historical values.
#' @param changes_pctl A `data.table` with predicted changes by initialvalue and pctl.
#' @param path_speed Data table  with speed paths
#'
#' @return A named list with one or both of `percentile_path` and `speed_path`.
#' @export
path_historical <- function(percentiles      = TRUE,
                            speed            = TRUE,
                            data_his,
                            eval_from        = NULL,
                            eval_to          = NULL,
                            granularity      = 0.1,
                            min              = 0,
                            max              = 100,
                            best             = NULL,
                            sequence_pctl    = seq(20, 80, 20),
                            changes_pctl     = NULL,
                            sequence_speed   = c(0.25, 0.5, 1, 2, 4),
                            path_speed       = NULL,
                            verbose          = TRUE) {


  # Input checks
  if (speed && is.null(path_speed)) {
    cli::cli_abort("{.arg path_speed} must be provided when {.arg speed} is TRUE.")
  }

  if (percentiles && is.null(changes_pctl)) {
    cli::cli_abort("{.arg changes_pctl} must be provided when {.arg percentiles} is TRUE.")
  }


  out <- list()

  if (percentiles) {

    out$pctl <- project_pctls_path(
      data_his         = data_his,
      eval_from        = eval_from,
      eval_to          = eval_to,
      granularity      = granularity,
      min              = min,
      max              = max,
      sequence_pctl    = sequence_pctl,
      changes_pctl     = changes_pctl,
      verbose          = verbose
    )
  }

  if (speed) {

    out$speed <- project_path_speed(
      data_his          = data_his,
      sequence_speed    = sequence_speed,
      path_speed        = path_speed,
      granularity       = granularity,
      eval_from         = eval_from,
      eval_to           = eval_to,
      min               = min,
      max               = max,
      best              = best
    )
  }

  if (length(out) == 0) {
    cli::cli_abort("At least one of `percentiles` or `speed` must be TRUE.")
  }

  return(out)
}


#' Historical percentiles paths
#'
#' Simulates year-by-year projected paths of a variable across percentiles, based on historical values and predicted changes
#'
#' @param data_his A `data.table` containing historical values with variables `code`, `year`, and `y_his`.
#' @inheritParams get_his_data
#' @inheritParams predict_pctls
#' @param sequence_pctl Numeric vector of percentile paths to calculate.
#'                      The percentile score is determined by the granularity of the percentiles chosen here. For example, if seq(20,80,20) is chosen, then a country’s progress will fall in the 0th-20th percentile, 20th-40th percentile etc.
#'                      This argument is only relevant if `percentiles = TRUE`.
#' @param changes_pctl A `data.table` with predicted changes by `initialvalue` and `pctl`.
#' @param verbose Logical. Whether to print progress messages.
#'
#' @return A `data.table` with projected values `y_pctl` by `code`, `year`, and `pctl`. `y_pctl` is the projected value along the given percentile path in that year
#'
#' @export
project_pctls_path <- function(data_his,
                               eval_from      = 2000,
                               eval_to        = 2022,
                               granularity    = 0.1,
                               min            = NULL,
                               max            = NULL,
                               sequence_pctl  = seq(20, 80, 20),
                               changes_pctl,
                               verbose        = TRUE) {
  # Input validation
  if (!inherits(data_his, "data.table")) {
    cli::cli_abort("Input data must be a data.table")
  }


  # Filter data_his within min/max if provided
  if (!is.null(min)) data_his <- data_his[y >= min]
  if (!is.null(max)) data_his <- data_his[y <= max]

  # Step 1: Expand data_his at start_year by pctl
  dt_expanded <- data_his[year == eval_from, .(code, year, y_his)]

  dt_expanded <- dt_expanded[, .(pctl = sequence_pctl),
                             by = .(code, year, y_his)]

  # Step 2: Add rows for future years
  all_years <- seq(eval_from,
                   eval_to)

  dt_expanded <- dt_expanded[, .(year = all_years),
                             by = .(code, pctl)][
    dt_expanded, on = .(code, pctl, year), y_his := i.y_his]

  setorder(dt_expanded,
           code,
           pctl,
           year)

  # # Step 3: Project year by year
  for (i in 2:length(all_years)) {

    prev_year <- all_years[i - 1]
    this_year <- all_years[i]


    prev <- dt_expanded[year == prev_year,
                        .(code, pctl, y_his)]

    prev <- joyn::left_join(prev,
                            changes_pctl,
                            by           = c("y_his = y", "pctl"),
                            relationship = "many-to-many",
                            verbose      = FALSE)

  #   # Apply change and granularity, and floor/ceiling bounds
    prev[, new_y := round((y_his + change) / granularity) * granularity]


    # Assign updated y_his to this year
    dt_expanded[year == this_year,
                y_his := prev[.SD, on = .(code, pctl),
                              x.new_y]]

    setorder(dt_expanded,
             code,
             year)

  } # end of for loop

  dt_expanded <- joyn::left_join(

    dt_expanded,
    data_his[, .(code, year, y)],       # only merge 'y'
    by           = c("code", "year"),
    relationship = "many-to-one",
    verbose      = FALSE,
    reportvar    = FALSE
  )

  setnames(dt_expanded,
           "y_his",
           "y_pctl")

  return(dt_expanded[])

}

