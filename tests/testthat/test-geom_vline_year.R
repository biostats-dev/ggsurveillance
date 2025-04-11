test_that("geom_vline_year handles basic functionality", {
  test_dates <- data.frame(
    date = as.Date("2023-12-01") + 0:60
  )

  # Test default year break (January 1)
  p <- ggplot(test_dates, aes(x = date)) +
    geom_bar() +
    geom_vline_year()

  expect_no_error(p)
  expect_s3_class(p, "ggplot")
})

test_that("geom_vline_year handles datetime data", {
  test_datetime <- data.frame(
    datetime = as.POSIXct("2023-12-15") + seq(0, 86400 * 30, by = 3600)
  )

  p <- ggplot(test_datetime, aes(x = datetime)) +
    geom_bar() +
    geom_vline_year()
  expect_no_error(p)
  expect_s3_class(p, "ggplot")
})

test_that("geom_vline_year works with break_type='week'", {
  test_dates <- data.frame(
    date = as.Date("2023-11-15") + 0:90
  )

  # Test with week number format
  p <- ggplot(test_dates, aes(x = date)) +
    geom_bar() +
    geom_vline_year(break_type = "week", year_break = "W01")

  expect_no_error(p)
  expect_s3_class(p, "ggplot")

  # Test with MM-DD format that gets converted to week
  p2 <- ggplot(test_dates, aes(x = date)) +
    geom_bar() +
    geom_vline_year(break_type = "week", year_break = "01-01")

  expect_no_error(p2)
  expect_s3_class(p2, "ggplot")

  # Test with mid-year week
  p3 <- ggplot(test_dates, aes(x = date)) +
    geom_bar() +
    geom_vline_year(break_type = "week", year_break = "W26")

  expect_no_error(p3)
  expect_s3_class(p3, "ggplot")

  # Test calc MM-DD from week
  p4 <- ggplot(test_dates, aes(x = date)) +
    geom_bar() +
    geom_vline_year(break_type = "day", year_break = "W26")

  expect_no_error(p4)
  expect_s3_class(p4, "ggplot")
})

test_that("geom_vline_year works with break_type='isoweek' and break_type='epiweek'", {
  test_dates <- data.frame(
    date = as.Date("2023-11-15") + 0:90
  )

  p <- ggplot(test_dates, aes(x = date)) +
    geom_bar() +
    geom_vline_year(break_type = "isoweek", year_break = "W01")

  expect_no_error(p)
  expect_s3_class(p, "ggplot")

  p1 <- ggplot(test_dates, aes(x = date)) +
    geom_bar() +
    geom_vline_year(break_type = "epiweek", year_break = "W01")

  expect_no_error(p1)
  expect_s3_class(p1, "ggplot")

  # Test US epidemiological week for influenza season (week 40)
  p2 <- ggplot(test_dates, aes(x = date)) +
    geom_bar() +
    geom_vline_year(break_type = "epiweek", year_break = "W40")

  expect_no_error(p2)
  expect_s3_class(p2, "ggplot")
})

test_that("geom_hline_year works with flipped coordinates", {
  test_dates <- data.frame(
    date = as.Date("2023-12-01") + 0:60
  )

  p <- ggplot(test_dates, aes(y = date)) +
    geom_bar() +
    geom_hline_year()
  expect_no_error(p)
  expect_s3_class(p, "ggplot")

  p1 <- ggplot(test_dates, aes(y = date)) +
    geom_bar() +
    geom_hline_year(break_type = "day", year_break = "01-01")

  expect_no_error(p1)
  expect_s3_class(p1, "ggplot")

  # Test week break
  p2 <- ggplot(test_dates, aes(y = date)) +
    geom_bar() +
    geom_hline_year(break_type = "week", year_break = "W01")

  expect_no_error(p2)
  expect_s3_class(p2, "ggplot")

  # Test isoweek break
  p3 <- ggplot(test_dates, aes(y = date)) +
    geom_bar() +
    geom_hline_year(break_type = "isoweek", year_break = "W01")

  expect_no_error(p3)
  expect_s3_class(p3, "ggplot")

  # Test epiweek break
  p4 <- ggplot(test_dates, aes(y = date)) +
    geom_bar() +
    geom_hline_year(break_type = "epiweek", year_break = "W01")

  expect_no_error(p4)
  expect_s3_class(p4, "ggplot")
})

test_that("calc_visible_years handles date ranges correctly", {
  # Test basic date range spanning multiple years
  range <- as_date(c("2023-06-01", "2025-06-01"))
  years <- .calc_visible_years(as.numeric(range), is_date = TRUE)
  expect_equal(years, as.numeric(as_date(c("2024-01-01", "2025-01-01"))))

  years <- .calc_visible_years(as.numeric(range), is_date = TRUE, year_break = "W01")
  expect_equal(years, as.numeric(as_date(c("2024-01-01", "2025-01-01"))))

  years <- .calc_visible_years(as.numeric(range), is_date = TRUE, break_type = "week")
  expect_equal(years, as.numeric(as_date(c("2024-01-01", "2024-12-30"))))

  years <- .calc_visible_years(as.numeric(range), is_date = TRUE, break_type = "week", year_break = "W01")
  expect_equal(years, as.numeric(as_date(c("2024-01-01", "2024-12-30"))))

  expect_error(
    .calc_visible_years(as.numeric(range), is_date = TRUE, break_type = "week", year_break = "test")
  )

  expect_warning(expect_warning( # 2 warnings need to expect_warning() calls.
    .calc_visible_years(as.numeric(range), is_date = TRUE, break_type = "day", year_break = "test")
  ))

  # Test date range within single year
  range <- as_date(c("2023-03-01", "2023-09-01"))
  years <- .calc_visible_years(as.numeric(range), is_date = TRUE)
  expect_equal(length(years), 0) # No year breaks visible

  # Test custom year break (fiscal year starting April 1)
  range <- as_date(c("2023-01-01", "2024-12-31"))
  years <- .calc_visible_years(as.numeric(range), is_date = TRUE, year_break = "04-01")
  expect_equal(years, as.numeric(as_date(c("2023-04-01", "2024-04-01"))))
})

test_that("calc_visible_years handles datetime ranges correctly", {
  # Test datetime range spanning multiple years
  range <- as_datetime(c("2023-12-31 18:00:00", "2024-01-01 06:00:00"))
  years <- .calc_visible_years(as.numeric(range), is_date = FALSE)
  expect_equal(years, as.numeric(as_datetime("2024-01-01 00:00:00")))

  # Test datetime range within single year
  range <- as_datetime(c("2023-06-01 00:00:00", "2023-12-30 23:59:59"))
  years <- .calc_visible_years(as.numeric(range), is_date = FALSE)
  expect_equal(length(years), 0) # No year breaks visible
})

test_that("calc_visible_years handles edge cases", {
  # Test range exactly at year boundaries, years are inclusive at borders
  range <- as_date(c("2023-01-01", "2024-01-01"))
  years <- .calc_visible_years(as.numeric(range), is_date = TRUE)
  expect_equal(years, as.numeric(as_date(c("2023-01-01", "2024-01-01"))))

  # Test very short range around new year
  range <- as_date(c("2023-12-31", "2024-01-01"))
  years <- .calc_visible_years(as.numeric(range), is_date = TRUE)
  expect_equal(years, as.numeric(as_date("2024-01-01")))
})

test_that("calc_week_from_mm_dd calculates week numbers correctly", {
  # Test beginning of the year
  expect_equal(.calc_week_from_mm_dd("01-01"), 1)
  expect_equal(.calc_week_from_mm_dd("01-07"), 1)

  # Test middle weeks
  expect_equal(.calc_week_from_mm_dd("02-15"), 7)
  expect_equal(.calc_week_from_mm_dd("06-30"), 26)

  # Test end of year
  expect_equal(.calc_week_from_mm_dd("12-25"), 52)
  expect_equal(.calc_week_from_mm_dd("12-31"), 53)

  # Test leap year day
  expect_equal(.calc_week_from_mm_dd("02-29"), 9)
})

test_that("calc_mm_dd_from_week converts weeks to month-day format", {
  # Test beginning of the year
  expect_equal(.calc_mm_dd_from_week("W01"), "01-01")

  # Test middle weeks
  expect_equal(.calc_mm_dd_from_week("W10"), "03-04")
  expect_equal(.calc_mm_dd_from_week("W25"), "06-17")

  # Test end of year
  expect_equal(.calc_mm_dd_from_week("W52"), "12-23")
  expect_equal(.calc_mm_dd_from_week("W53"), "12-30")
})
