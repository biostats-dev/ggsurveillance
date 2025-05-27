test_that("align_dates_seasonal handles different input types correctly", {
  # Test date vector input
  dates <- seq(as.Date("2020-01-01"), as.Date("2024-03-01"), by = "week")
  expect_s3_class(
    align_dates_seasonal(dates, date_resolution = "week"),
    "data.frame"
  )

  # Test data.frame input with quoted column name
  df <- data.frame(date = dates)
  expect_s3_class(
    align_dates_seasonal(df, dates_from = "date", date_resolution = "week"),
    "data.frame"
  )

  # Test data.frame input with unquoted column name
  expect_s3_class(
    align_dates_seasonal(df, dates_from = date, date_resolution = "week"),
    "data.frame"
  )
})

test_that("align_dates_seasonal handles different date formats", {
  # Create test data with different date formats
  dates_df <- data.frame(
    iso_date = "2024-03-09",
    month = "2024-03",
    week = "2024-W10",
    week_day = "2024-W10-1"
  )

  # Test each format
  expect_no_error(align_dates_seasonal(dates_df, dates_from = iso_date))
  expect_no_error(align_dates_seasonal(dates_df, dates_from = month))
  expect_no_error(align_dates_seasonal(dates_df, dates_from = week))
  expect_no_error(align_dates_seasonal(dates_df, dates_from = week_day))
})

test_that("align_dates_seasonal handles different date resolutions", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2024-03-01"), by = "day")
  df <- data.frame(date = dates)

  # Test each resolution
  expect_no_error(align_dates_seasonal(df, dates_from = date, date_resolution = "week"))
  expect_no_error(align_dates_seasonal(df, dates_from = date, date_resolution = "isoweek"))
  expect_no_error(align_dates_seasonal(df, dates_from = date, date_resolution = "epiweek"))
  expect_no_error(align_dates_seasonal(df, dates_from = date, date_resolution = "month"))
  expect_no_error(align_dates_seasonal(df, dates_from = date, date_resolution = "day"))
})

test_that("align_dates_seasonal correctly identifies current season", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2024-03-01"), by = "week")
  result <- align_dates_seasonal(dates, date_resolution = "week")

  # Check that the most recent season is marked as current
  expect_true(result$current_season[which.max(result$date)])
})

test_that("align_dates_seasonal handles leap weeks correctly", {
  # Create data that includes week 53
  dates <- seq(as.Date("2020-01-01"), as.Date("2024-03-01"), by = "week")

  # Test with drop_leap_week = TRUE
  result_drop <- align_dates_seasonal(
    dates,
    date_resolution = "week",
    drop_leap_week = TRUE
  )
  expect_false(any(result_drop$week == "53" & !result_drop$current_season))

  # Test with drop_leap_week = FALSE
  result_keep <- align_dates_seasonal(
    dates,
    date_resolution = "week",
    drop_leap_week = FALSE
  )
  expect_true(any(result_keep$week == "53"))
})

test_that("align_dates_seasonal handles custom start points", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2024-03-01"), by = "week")

  # Test custom starts for different resolutions
  expect_no_error(
    align_dates_seasonal(dates, date_resolution = "week", start = 40)
  )
  expect_no_error(
    align_dates_seasonal(dates, date_resolution = "month", start = 9)
  )
  expect_no_error(
    align_dates_seasonal(dates, date_resolution = "day", start = 200)
  )
})

test_that("align_dates_seasonal handles custom target years", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2024-03-01"), by = "week")

  expect_no_error(result <- align_dates_seasonal(
    dates,
    date_resolution = "week",
    target_year = 2020
  ))

  # Check that aligned dates are in target year or target year + 1
  expect_true(all(
    lubridate::year(result$date_aligned) %in% c(2020, 2021)
  ))
})

test_that("align_dates_seasonal errors on invalid inputs", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2024-03-01"), by = "week")

  # Test invalid date resolution
  expect_error(
    align_dates_seasonal(dates, date_resolution = "invalid")
  )

  # Test invalid date format
  invalid_dates <- data.frame(date = c("invalid", "dates"))
  expect_error(
    align_dates_seasonal(invalid_dates, dates_from = date)
  )
})

test_that("align_dates_seasonal errors on invalid inputs", {
  dates <- seq(as.Date("2020-01-01"), as.Date("2024-03-01"), by = "week")

  # Test invalid date resolution
  expect_error(
    align_dates_seasonal(dates, date_resolution = "invalid")
  )

  # Test invalid date format
  invalid_dates <- data.frame(date = c("invalid", "dates"))
  expect_error(
    align_dates_seasonal(invalid_dates, dates_from = date)
  )
})
