

# ------------------------------
# Setup dummy data
# ------------------------------
# NOTE: `data_fut` mirrors the real output of `prep_data_fut()`, which keeps
# exactly one row per country (the latest observation), with both `y` and
# `y_fut` populated. Using multiple rows per country here does not reflect
# real usage and breaks the 1:1 joins inside `future_path_speed()` /
# `path_future_his_speed()`.

data_fut <- data.table(
  code  = c("A", "B"),
  year  = c(2021, 2021),
  y     = c(52, 61),
  y_fut = c(52, 61)
)

# Predicted changes table for percentiles
changes_pctl <- data.table(
  y_fut  = 52,  # must match data_fut column for join
  pctl   = 20,
  change = 1
)

# Dummy speed path with regularly-spaced integer `time` steps
path_speed <- data.table(
  time = 0:2,
  y    = c(52, 53, 54)
)

# Historical scores
scores <- data.table(
  code  = c("A", "B"),
  year  = c(2021, 2021),
  score = c(1, 2)
)

# ------------------------------
# Tests
# ------------------------------

test_that("future_path_pctls returns expected columns", {
  res <- future_path_pctls(
    data_fut      = data_fut,
    sequence_pctl = c(20, 50),
    changes_pctl  = changes_pctl,
    target_year   = 2022,
    granularity   = 0.1
  )

  expect_true(all(c("code", "year", "pctl", "y_pctl") %in% names(res)))
  expect_true(nrow(res) > 0)
})

test_that("future_path_speed handles empty future data gracefully", {
  empty_data <- data.table(code = character(), year = numeric(), y = numeric())
  res <- future_path_speed(
    data_fut       = empty_data,
    sequence_speed = c(1, 2),
    path_speed     = path_speed,
    target_year    = 2022
  )
  expect_true(nrow(res) == 0)
})

test_that("future_path_speed returns valid projections", {
  res <- future_path_speed(
    data_fut       = data_fut,
    sequence_speed = c(1, 2),
    path_speed     = path_speed,
    target_year    = 2022,
    min            = 0,
    max            = 100
  )
  expect_true(all(c("code", "year", "speed", "y_fut") %in% names(res)))
  expect_true(nrow(res) > 0)
})

test_that("future_path combines model-based and historical-speed projections", {
  res <- future_path(
    data_fut      = data_fut,
    sequence_speed = c(1, 2),
    path_speed     = path_speed,
    best           = "high",
    speed          = TRUE,
    percentiles    = FALSE,
    scores         = scores,
    target_year    = 2022
  )

  expect_true("speed" %in% names(res$speed))
  expect_true("speed_source" %in% names(res$speed))
  expect_true(nrow(res$speed) > 0)
  expect_true(any(res$speed$speed_source == "historical"))
  expect_true(any(res$speed$speed_source == "model"))
})

# ------------------------------------------------------------------
# Regression tests: fractional-year re-gridding (see `regrid_fut_years()`)
# ------------------------------------------------------------------
#
# `future_path_speed()` / `path_future_his_speed()` compute a continuous
# (fractional) `year` for each simulated step, based on how long the model
# predicts it takes to move between indicator levels. With irregularly
# spaced `time` steps in `path_speed` (the realistic case, since these come
# from a fitted quantile regression), those fractional years essentially
# never line up exactly with whole calendar years. The old implementation
# filtered for rows landing exactly on integer years, which silently
# discarded almost everything and made projections appear to stop far short
# of `target_year`. These tests guard against that regression.

# Irregular, non-integer-aligned time steps
path_speed_irregular <- data.table(
  time = cumsum(c(0, 0.37, 0.81, 1.4, 2.9, 3.05, 5.6, 7.9, 11.2, 15.9, 22.4, 30.1)),
  y    = seq(52, 74, by = 2)
)

test_that("future_path_speed reaches a far target_year with irregular time steps", {
  far_target <- 2150

  res <- future_path_speed(
    data_fut       = data_fut,
    sequence_speed = c(1, 2),
    path_speed     = path_speed_irregular,
    target_year    = far_target,
    min            = 0,
    max            = 200
  )

  expect_true(nrow(res) > 0)

  # Every code/speed group should reach the requested target year
  max_years <- res[, .(maxyear = max(year)), by = .(code, speed)]
  expect_true(all(max_years$maxyear == far_target))

  # Every code/speed group should have exactly one row per whole calendar
  # year from the starting year through target_year (no gaps from rows
  # being dropped for not landing on an integer year).
  counts <- res[, .N, by = .(code, speed)]
  expect_true(all(counts$N == far_target - min(data_fut$year) + 1))

  # All year values should be whole numbers
  expect_true(all(res$year == round(res$year)))
})

test_that("path_future_his_speed reaches a far target_year with irregular time steps", {
  far_target <- 2150

  res <- path_future_his_speed(
    data_fut    = data_fut,
    scores      = scores,
    path_speed  = path_speed_irregular,
    best        = "high",
    target_year = far_target,
    min         = 0,
    max         = 200
  )

  expect_true(nrow(res) > 0)

  max_years <- res[, .(maxyear = max(year)), by = code]
  expect_true(all(max_years$maxyear == far_target))

  counts <- res[, .N, by = code]
  expect_true(all(counts$N == far_target - min(data_fut$year) + 1))

  expect_true(all(res$year == round(res$year)))
})

test_that("regrid_fut_years interpolates onto whole years and extrapolates flat", {
  # Fractional years, not aligned to whole numbers
  year <- c(2021, 2021.6, 2023.2, 2025.9)
  y    <- c(10, 20, 30, 40)

  out <- regrid_fut_years(year, y, target_year = 2028)

  expect_s3_class(out, "data.table")
  expect_identical(out$year, seq(2021, 2028, 1))
  # No NAs: interpolated within range, held flat (rule = 2) beyond it
  expect_false(anyNA(out$y_fut))
  # Flat extrapolation past the last observed fractional year
  expect_equal(out$y_fut[out$year == 2028], 40)
  # Values should be non-decreasing given monotonically increasing y
  expect_true(all(diff(out$y_fut) >= 0))
})

test_that("regrid_fut_years handles a single usable data point", {
  out <- regrid_fut_years(year = 2021, y = 42, target_year = 2025)

  expect_identical(out$year, seq(2021, 2025, 1))
  expect_true(all(out$y_fut == 42))
})
