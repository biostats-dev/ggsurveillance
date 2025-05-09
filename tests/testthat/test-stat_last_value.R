test_that("stat_last_value creates points at the last value of lines", {
  # Basic test with economics data
  p1 <- ggplot(economics, aes(x = date, y = unemploy)) +
    geom_line() +
    stat_last_value()

  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  vdiffr::expect_doppelganger("1_stat_last_value_basic", p1)

  # Test with multiple lines (economics_long)
  p2 <- ggplot(economics_long, aes(x = date, y = value, color = variable)) +
    geom_line() +
    stat_last_value()

  expect_s3_class(p2, "ggplot")
  expect_no_error(p2)
  vdiffr::expect_doppelganger("1_stat_last_value_multiple_lines", p2)
})

test_that("geom_text_last_value adds text at the last value", {
  # Basic text label
  p1 <- ggplot(economics, aes(x = date, y = unemploy)) +
    geom_line() +
    geom_text_last_value()

  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  vdiffr::expect_doppelganger("2_geom_text_last_value_basic", p1)

  # Test with labeller
  p2 <- ggplot(economics, aes(x = date, y = unemploy)) +
    geom_line() +
    geom_text_last_value(labeller = scales::label_number())

  expect_s3_class(p2, "ggplot")
  expect_no_error(p2)
  vdiffr::expect_doppelganger("2_geom_text_last_value_labeller", p2)

  # Test with percentage labeller
  p3 <- ggplot(economics, aes(x = date, y = unemploy / pop)) +
    geom_line() +
    geom_text_last_value(labeller = scales::label_percent(accuracy = 0.1))

  expect_s3_class(p3, "ggplot")
  expect_no_error(p3)
  vdiffr::expect_doppelganger("2_geom_text_last_value_percent", p3)
})

test_that("geom_label_last_value adds labels at the last value", {
  # Basic label
  p1 <- ggplot(economics, aes(x = date, y = unemploy)) +
    geom_line() +
    geom_label_last_value()

  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  vdiffr::expect_doppelganger("3_geom_label_last_value_basic", p1)

  # Test with custom nudging
  p2 <- ggplot(economics, aes(x = date, y = unemploy)) +
    geom_line() +
    geom_label_last_value(nudge_rel = 0.1, expand_rel = 0.1)

  expect_s3_class(p2, "ggplot")
  expect_no_error(p2)
  vdiffr::expect_doppelganger("3_geom_label_last_value_nudge", p2)
})

test_that("geom_text_last_value_repel works with ggrepel", {
  skip_if_not_installed("ggrepel")

  # Basic repel text
  p1 <- ggplot(economics_long, aes(x = date, y = value, color = variable)) +
    geom_line() +
    geom_text_last_value_repel(aes(label = variable))

  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  vdiffr::expect_doppelganger("4_geom_text_last_value_repel_basic", p1)

  # Test with min.segment.length parameter
  p2 <- ggplot(economics_long, aes(x = date, y = value, color = variable)) +
    geom_line() +
    geom_text_last_value_repel(aes(label = variable), min.segment.length = 1)

  expect_s3_class(p2, "ggplot")
  expect_no_error(p2)
  vdiffr::expect_doppelganger("4_geom_text_last_value_repel_min_segment", p2)
})

test_that("geom_label_last_value_repel works with ggrepel", {
  skip_if_not_installed("ggrepel")

  # Basic repel label
  p1 <- ggplot(economics_long, aes(x = date, y = value, color = variable)) +
    geom_line() +
    geom_label_last_value_repel(aes(label = variable))

  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  vdiffr::expect_doppelganger("5_geom_label_last_value_repel_basic", p1)

  # Test with custom expand and nudge
  p2 <- ggplot(economics_long, aes(x = date, y = value, color = variable)) +
    geom_line() +
    geom_label_last_value_repel(aes(label = variable), expand_rel = 0.1, nudge_rel = 0.2)

  expect_s3_class(p2, "ggplot")
  expect_no_error(p2)
  vdiffr::expect_doppelganger("5_geom_label_last_value_repel_custom", p2)
})

test_that("stat_last_value works with absolute nudging", {
  # Test with datetime values and absolute nudging
  df <- influenza_germany |>
    dplyr::mutate(date = as.POSIXct(.coerce_to_date(ReportingWeek)))

  p1 <- ggplot(df, aes(x = date, y = Incidence, color = AgeGroup)) +
    geom_line() +
    stat_last_value() +
    geom_text_last_value_repel(
      nudge_add = 300 * (60 * 60 * 24), expand_add = 200 * (60 * 60 * 24),
      nudge_rel = 0, expand_rel = 0
    )

  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  vdiffr::expect_doppelganger("6_stat_last_value_abs_nudge_datetime", p1)

  # Test with date values and absolute nudging
  df <- influenza_germany |>
    dplyr::mutate(date = .coerce_to_date(ReportingWeek))
  p2 <- ggplot(df, aes(x = date, y = Incidence, color = AgeGroup)) +
    geom_line() +
    stat_last_value() +
    geom_text_last_value_repel(
      nudge_add = 365, expand_add = 500,
      nudge_rel = 0, expand_rel = 0
    )

  expect_s3_class(p2, "ggplot")
  expect_no_error(p2)
  vdiffr::expect_doppelganger("6_stat_last_value_abs_nudge_date", p2)
})

test_that("stat_last_value handles NA values correctly", {
  # Create data with NA values
  df <- data.frame(
    x = 1:10,
    y = c(1, 2, 3, NA, 5, 6, 7, 8, 9, NA)
  )

  p1 <- ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    stat_last_value()

  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  expect_warning(expect_warning(vdiffr::expect_doppelganger("7_stat_last_value_na_values", p1)))

  # Test with NA at the end
  df2 <- data.frame(
    x = 1:10,
    y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, NA)
  )

  p2 <- ggplot(df2, aes(x = x, y = y)) +
    geom_line() +
    stat_last_value() +
    geom_label_last_value()

  expect_s3_class(p2, "ggplot")
  expect_no_error(p2)
  vdiffr::expect_doppelganger("7_stat_last_value_na_at_end", p2) |>
    expect_warning() |>
    expect_warning() |>
    expect_warning()
})

test_that("StatLastValue$compute_group correctly processes data", {
  # Create test data
  test_data <- data.frame(
    x = 1:5,
    y = c(10, 20, 30, 40, 50),
    flipped_aes = FALSE
  )

  # Test with basic parameters
  result <- StatLastValue$compute_group(
    data = test_data,
    scales = list(x = list(range = list(range = c(0, 5)))),
    flipped_aes = FALSE,
    nudge_rel = 0.01,
    nudge_add = 0,
    expand_rel = 0.02,
    expand_add = 0,
    labeller = identity
  )

  # Check structure of result
  expect_s3_class(result, "data.frame")
  expect_true(all(c("x", "y", "x0", "xmax", "label_formatted") %in% names(result)))
  expect_equal(result$x0, 5)
  expect_equal(result$x, 5 + 0.01 * 5)
  expect_equal(result$y, 50)
  expect_equal(result$xmax, 5 + 0.01 * 5 + 0.02 * 5)
  expect_equal(result$label_formatted, 50)
})

test_that("StatLastValueRepel$compute_group correctly processes data", {
  # Create test data
  test_data <- data.frame(
    x = 1:5,
    y = c(10, 20, 30, 40, 50),
    flipped_aes = FALSE
  )

  # Test with basic parameters
  result <- StatLastValueRepel$compute_group(
    data = test_data,
    scales = list(x = list(range = list(range = c(0, 5)))),
    flipped_aes = FALSE,
    nudge_rel = 0.01,
    nudge_add = 0,
    expand_rel = 0.02,
    expand_add = 0,
    labeller = scales::label_percent()
  )

  # Check structure of result
  expect_s3_class(result, "data.frame")
  expect_true(all(c("x", "y", "x0", "nudge_x", "xmax", "label_formatted") %in% names(result)))
  expect_equal(result$x0, 5)
  expect_equal(result$x, 5)
  expect_equal(result$y, 50)
  expect_equal(result$nudge_x, 5 + 0.01 * 5)
  expect_equal(result$xmax, 5 + 0.01 * 5 + 0.02 * 5)
  expect_equal(result$label_formatted, "5 000%")
})
