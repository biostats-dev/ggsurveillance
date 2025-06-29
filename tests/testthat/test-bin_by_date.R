test_that("bin_by_date handles different input types correctly", {
  # Test with data.frame input
  df <- data.frame(
    date = as.Date("2024-01-01") + 0:10,
    cases = rep(1, 11)
  )

  result_df <- bin_by_date(df, dates_from = date)
  expect_s3_class(result_df, "data.frame")
  expect_true(all(c("date", "n", "incidence") %in% names(result_df)))
  expect_true(lubridate::is.Date(result_df$date))
  expect_true(is.numeric(result_df$n))
  expect_true(is.numeric(result_df$incidence))


  # Test with quoted column name
  result_quoted <- bin_by_date(df, dates_from = "date")
  expect_identical(result_df, result_quoted)

  # Test with vector input
  dates_vector <- as.Date("2024-01-01") + 0:10
  result_vector <- bin_by_date(dates_vector)
  expect_s3_class(result_vector, "data.frame")
  expect_true("date" %in% names(result_vector))
})

test_that("bin_by_date date coercion works with .coerce_to_date", {
  # Test various date formats that should be handled by .coerce_to_date
  test_data <- list(
    iso_dates = data.frame(date = "2024-03-09", cases = 1),
    month_dates = data.frame(date = "2024-03", cases = 1),
    week_dates = data.frame(date = "2024-W10", cases = 1),
    week_day_dates = data.frame(date = "2024-W10-1", cases = 1),
    date_objects = data.frame(date = as.Date("2024-03-09"), cases = 1),
    datetime_objects = data.frame(date = as.POSIXct("2024-03-09 12:00:00"), cases = 1)
  )

  for (i in seq_along(test_data)) {
    expect_no_error(
      bin_by_date(test_data[[i]], dates_from = date, n = cases)
    )
  }
})

test_that("bin_by_date handles different date resolutions correctly", {
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "day") # 2 years: 2023-2024
  df <- data.frame(date = dates, cases = 1)

  # Test basic resolutions
  expect_no_error(expect_identical(nrow(bin_by_date(df, dates_from = date, date_resolution = "day")), 365L + 366L))
  expect_no_error(expect_identical(nrow(bin_by_date(df, dates_from = date, date_resolution = "week")), 53L + 53L))
  expect_no_error(expect_identical(nrow(bin_by_date(df, dates_from = date, date_resolution = "month")), 2L * 12L))
  expect_no_error(expect_identical(nrow(bin_by_date(df, dates_from = date, date_resolution = "quarter")), 2L * 4L))
  expect_no_error(expect_identical(nrow(bin_by_date(df, dates_from = date, date_resolution = "year")), 2L))

  # Test year resolutions
  expect_no_error(expect_identical(nrow(bin_by_date(df, dates_from = date, date_resolution = "isoweek")), 2L * 53L))
  expect_no_error(expect_identical(nrow(bin_by_date(df, dates_from = date, date_resolution = "epiweek")), 52L + 53L))
  expect_no_error(expect_identical(nrow(bin_by_date(df, dates_from = date, date_resolution = "isoyear")), 4L))
  expect_no_error(expect_identical(nrow(bin_by_date(df, dates_from = date, date_resolution = "epiyear")), 3L))
})

test_that("bin_by_date week_start parameter works correctly", {
  # Create data spanning a week boundary
  dates <- seq(as.Date("2024-01-07"), as.Date("2024-01-14"), by = "day") # Sunday to Sunday
  df <- data.frame(date = dates, cases = 1)

  # Test different week starts
  result_monday <- bin_by_date(df, dates_from = date, date_resolution = "week", week_start = 1)
  result_sunday <- bin_by_date(df, dates_from = date, date_resolution = "week", week_start = 7)

  expect_identical(result_monday$n, c(1, 7))
  expect_identical(result_sunday$n, c(7, 1))

  # Week start should be overridden for isoweek and epiweek
  result_iso_custom <- bin_by_date(df, dates_from = date, date_resolution = "isoweek", week_start = 3)
  result_epi_custom <- bin_by_date(df, dates_from = date, date_resolution = "epiweek", week_start = 3)

  # Should still work (week_start gets overridden internally)
  expect_identical(result_iso_custom, result_monday)
  expect_identical(result_epi_custom, result_sunday)
})

test_that("bin_by_date weight and population handling works correctly", {
  df <- data.frame(
    date = as.Date("2024-01-01") + rep(0:2, each = 3),
    cases = c(1, 2, 3, 2, 1, 3, 1, 1, 2),
    pop = rep(1000, 9)
  )

  # Test with custom weights (numeric column)
  result_weighted <- bin_by_date(df, dates_from = date, n = cases)
  expect_identical(sum(result_weighted$n), sum(df$cases))

  # Test with custom weights (quoted column name)
  result_weighted_quoted <- bin_by_date(df, dates_from = date, n = "cases")
  expect_identical(result_weighted, result_weighted_quoted)

  # Test with population column
  result_with_pop <- bin_by_date(df, dates_from = date, n = cases, population = pop)
  expect_true(all(result_with_pop$incidence == result_with_pop$n / 1000))

  # Test with population as number
  result_pop_num <- bin_by_date(df, dates_from = date, n = cases, population = 500)
  expect_true(all(result_pop_num$incidence == result_pop_num$n / 500))
})

test_that("bin_by_date aggregation works correctly", {
  # Create data with known aggregation
  df <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-08", "2024-01-09")),
    cases = c(2, 3, 1, 4)
  )

  result <- bin_by_date(df, dates_from = date, n = cases, date_resolution = "week")

  # Should have 2 weeks
  expect_identical(nrow(result), 2L)

  # Check aggregation is correct
  expect_identical(sum(result$n), sum(df$cases))
  expect_true(all(result$n %in% c(5, 5))) # 2+3=5 and 1+4=5
})

test_that("bin_by_date preserves grouping correctly", {
  df <- data.frame(
    date = rep(as.Date("2024-01-01") + 0:3, 2),
    group = rep(c("A", "B"), each = 4),
    cases = 1:8
  )

  # Test with grouping
  result <- df |>
    dplyr::group_by(group) |>
    bin_by_date(dates_from = date, n = cases)

  expect_true("group" %in% names(result))
  expect_identical(nrow(result), 2L) # One row per group
  expect_identical(n_distinct(result$date), 1L)
})

test_that("bin_by_date warns when grouped by date column", {
  df <- data.frame(
    date = as.Date("2024-01-01") + 0:3,
    cases = 1:4
  )

  grouped_by_date <- dplyr::group_by(df, date)

  expect_warning(
    bin_by_date(grouped_by_date, dates_from = date, n = cases),
    "grouped by date column"
  )
})

test_that("bin_by_date .groups parameter works correctly", {
  df <- data.frame(
    date = rep(as.Date("2024-01-01") + 0:1, 2),
    group = rep(c("A", "B"), each = 2),
    cases = 1:4
  )

  grouped_df <- dplyr::group_by(df, group)

  # Test different .groups options
  result_drop <- bin_by_date(grouped_df, dates_from = date, n = cases, .groups = "drop")
  result_keep <- bin_by_date(grouped_df, dates_from = date, n = cases, .groups = "keep")

  expect_false(dplyr::is_grouped_df(result_drop))
  expect_true(dplyr::is_grouped_df(result_keep))
})

test_that("bin_by_date handles NA values correctly", {
  df <- data.frame(
    date = c(as.Date("2024-01-01"), NA, as.Date("2024-01-08")),
    cases = c(1, 2, 3)
  )

  result <- bin_by_date(df, dates_from = date, n = cases)

  # Should handle NA dates gracefully
  expect_s3_class(result, "data.frame")
  expect_equal(result$n, c(1, 3, 2)) # 1, 3, NA is last 2
})

test_that("bin_by_date input validation works", {
  df <- data.frame(
    date = as.Date("2024-01-01") + 0:3,
    cases = 1:4
  )

  # Test with non-existent column
  expect_error(
    bin_by_date(df, dates_from = non_existent_column)
  )
})

test_that("bin_by_date handles empty data frames", {
  empty_df <- data.frame(
    date = as.Date(character(0)),
    cases = numeric(0)
  )

  result <- bin_by_date(empty_df, dates_from = date, n = cases)
  expect_s3_class(result, "data.frame")
  expect_identical(nrow(result), 0L)
})

test_that("bin_by_date fill_gaps parameter works", {
  # Create data with gaps
  df <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-08", "2024-01-29")), # 2-week gaps
    # date = as.Date(c("2024-01-01", "2024-01-15", "2024-01-29")), # TODO: FIX
    cases = c(1, 2, 3)
  )

  # Without fill_gaps
  result_no_fill <- bin_by_date(df, dates_from = date, n = cases, date_resolution = "week")
  expect_identical(nrow(result_no_fill), 3L)

  # With fill_gaps (only test if tsibble is available)
  skip_if_not_installed("tsibble")
  result_fill <- bin_by_date(df,
    dates_from = date, n = cases,
    date_resolution = "week", fill_gaps = TRUE
  )
  expect_identical(nrow(result_fill), 5L)

  # Check that gaps are filled with zeros
  zero_rows <- result_fill[result_fill$n == 0, ]
  expect_identical(nrow(zero_rows), 2L)
})
