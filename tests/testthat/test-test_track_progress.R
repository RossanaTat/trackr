library(testthat)
library(data.table)
library(cli)

# ----------------------------- #
# Test dataset
# ----------------------------- #
test_data <- data.table(
  iso3c = rep(c("A","B","C","D"), each = 10),
  date  = rep(2010:2019, 4),
  indicator = c(
    seq(50, 59, 1),               # Country A
    c(NA, 40:48),                 # Country B (missing first year)
    seq(60, 69, 1),               # Country C
    rep(NA, 10)                   # Country D (all missing)
  )
)

# ----------------------------- #
# Track Progress tests
# ----------------------------- #
test_that("track_progress handles percentiles only", {

  res <- track_progress(
    data = test_data,
    indicator = "indicator",
    code_col = "iso3c",
    year_col = "date",
    eval_from = 2010,
    eval_to = 2019,
    speed = FALSE,
    percentiles = TRUE,
    future = FALSE,
    best = "high"
  )

  expect_true("data_model" %in% names(res))
  expect_true("path_historical" %in% names(res))
  expect_true("scores" %in% names(res))
  expect_null(res$path_future)
  expect_true(all(res$scores$pctl$code %in% c("A","B","C")))
})

# ----------------------------- #
# Track Progress with speeds only
# ----------------------------- #
test_that("track_progress handles speeds only", {

  # Use numeric sequence_speed
  res <- track_progress(
    data = test_data,
    indicator = "indicator",
    code_col = "iso3c",
    year_col = "date",
    eval_from = 2010,
    eval_to = 2019,
    speed = TRUE,
    percentiles = FALSE,
    future = FALSE,
    sequence_speed = c(0.5,1,2),
    best = "high"
  )

  expect_true("scores" %in% names(res))
  expect_true(all(res$scores$speed$code %in% c("A","B","C")))
  expect_false(any(res$scores$speed$code == "D"))  # Country D dropped (all missing)
})

# ----------------------------- #
# Track Progress with historical speeds
# ----------------------------- #
test_that("track_progress handles 'his' speed correctly", {

  res <- track_progress(
    data = test_data,
    indicator = "indicator",
    code_col = "iso3c",
    year_col = "date",
    eval_from = 2010,
    eval_to = 2019,
    speed = TRUE,
    percentiles = FALSE,
    future = FALSE,
    sequence_speed = c("his", 1, 2),
    best = "high"
  )

  # Check that historical speed appears
  expect_true("his" %in% unique(res$path_historical$speed$speed))
  # Countries with no historical speed (D) dropped
  expect_false(any(res$path_historical$speed$code == "D"))
})

# ----------------------------- #
# Track Progress with future projections
# ----------------------------- #
test_that("track_progress produces future paths", {

  res <- track_progress(
    data = test_data,
    indicator = "indicator",
    code_col = "iso3c",
    year_col = "date",
    eval_from = 2010,
    eval_to = 2019,
    speed = TRUE,
    percentiles = TRUE,
    future = TRUE,
    target_year = 2022,
    sequence_speed = c("his",1),
    sequence_pctl = seq(20,80,20),
    best = "high"
  )

  expect_true("path_future" %in% names(res))
  expect_true(!is.null(res$path_future$pctl))
  expect_true(!is.null(res$path_future$speed))
  expect_true(all(res$path_future$speed$year <= 2022))
  expect_true(all(res$path_future$pctl$year <= 2022))
})

# ----------------------------- #
# Edge cases: short evaluation period and missing historical speed
# ----------------------------- #
test_that("short evaluation period filters speed scores", {

  res <- track_progress(
    data = test_data,
    indicator = "indicator",
    code_col = "iso3c",
    year_col = "date",
    eval_from = 2019,
    eval_to = 2019,
    speed = TRUE,
    percentiles = FALSE,
    future = FALSE,
    sequence_speed = c("his",1,2),
    best = "high"
  )

  # No speed scores should be computed if period <5 years
  expect_true(all(res$scores$speed$evaluationperiod != "2019-2019"))
})

