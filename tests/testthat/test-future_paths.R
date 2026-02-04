

# ------------------------------
# Setup dummy data
# ------------------------------

data_fut <- data.table(
  code = rep(c("A", "B"), each = 2),
  year = c(2020, 2021, 2020, 2021),
  y    = c(50, 52, 60, 61)
)

# Predicted changes table for percentiles
changes_pctl <- data.table(
  y_fut  = 52,  # must match data_fut column for join
  pctl   = 20,
  change = 1
)

# Dummy speed path
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
    target_year    = 2022
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
