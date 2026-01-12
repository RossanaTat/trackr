# data <- wbstats::wb_data(
#     indicator = c("SH.DYN.NMRT"),
#     lang = "en",
#     country = "countries_only"
#   )



test_that("future projections work with a vector of speeds", {


  result <- track_progress(
    data = data,
    indicator = "SH.DYN.NMRT",
    code_col = "iso3c",
    year_col = "date",
    startyear_data = 1960,
    endyear_data = 2024,
    eval_from = 2018,
    eval_to = 2023,
    future = TRUE,
    target_year = 2050,
    speed = TRUE,
    percentiles = FALSE,
    sequence_speed = c(1, 1.5, 2),
    best = "low"
  )

  expect_true("path_future" %in% names(result))
  expect_true(is.data.frame(result$path_future))
  expect_true(nrow(result$path_future) > 0)
  expect_true(all(c("code", "year", "speed", "y_fut") %in% names(result$path_future)))
})

test_that("future projections work with country-specific historical speeds", {
  data <- wbstats::wb_data(
    indicator = c("SH.DYN.NMRT"),
    lang = "en",
    country = "countries_only"
  )

  result <- track_progress(
    data = data,
    indicator = "SH.DYN.NMRT",
    code_col = "iso3c",
    year_col = "date",
    startyear_data = 1960,
    endyear_data = 2024,
    eval_from = 2018,
    eval_to = 2023,
    future = TRUE,
    target_year = 2050,
    speed = TRUE,
    percentiles = FALSE,
    sequence_speed = NULL, # triggers country-specific speeds
    best = "low"
  )

  expect_true("path_future_his" %in% names(result))
  expect_true(is.data.frame(result$path_future_his))
  expect_true(nrow(result$path_future_his) > 0)
  expect_true(all(c("code", "year", "speed", "y_fut") %in% names(result$path_future_his)))
})
