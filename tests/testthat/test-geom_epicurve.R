test_that("geom_epicurve handles basic date inputs", {
  # Test data
  test_dates <- data.frame(
    date = as.Date("2024-01-01") + 0:10,
    cat = c(rep("A", 5), rep("B", 6))
  )

  # Create plot
  p <- ggplot(test_dates, aes(x = date, fill = cat)) +
    geom_vline_year() +
    geom_epicurve(date_resolution = "day") +
    stat_bin_date(aes(y = after_stat(count) * 1.05, label = after_stat(count)),
      date_resolution = "day", geom = "text"
    ) +
    scale_y_cases_5er()

  # Test that the plot is created successfully
  expect_s3_class(p, "ggplot")
})

test_that("geom_epicurve handles date_resolution = NA/NULL", {
  # Test data
  test_dates <- data.frame(
    date = as.Date("2024-01-01") + 0:10,
    cat = c(rep("A", 5), rep("B", 6))
  )

  # Create plot
  expect_no_error({
    ggplot(test_dates, aes(x = date)) +
      geom_vline_year() +
      geom_epicurve(aes(fill = cat), date_resolution = "week") +
      stat_bin_date(aes(y = after_stat(count) * 1.05, label = after_stat(count)),
        geom = "text", date_resolution = "week",
      ) +
      scale_y_cases_5er()
  })
})

test_that("geom_epicurve handles flipped aes", {
  # Test data
  test_dates <- data.frame(
    date = as.Date("2024-01-01") + 0:10,
    cat = c(rep("A", 5), rep("B", 6))
  )

  # Create plot
  p <- ggplot(test_dates, aes(y = date, fill = cat)) +
    geom_hline_year() +
    geom_epicurve(date_resolution = "day") +
    scale_x_cases_5er(n = 10, n.min = 9, u5.bias = 3)

  # Test that the plot is created successfully
  expect_s3_class(p, "ggplot")
})

test_that("geom_epicurve handles datetime data", {
  test_datetime <- data.frame(
    datetime = as.POSIXct("2024-01-01") + seq(0, 86400 * 20, by = 86400)
  )

  p <- ggplot(test_datetime, aes(x = datetime)) +
    geom_vline_year() +
    geom_epicurve(date_resolution = "day") +
    scale_y_cases_5er(n = 5, min.n = 4, u5.bias = 10)

  expect_s3_class(p, "ggplot")
  expect_no_error({
    p
  })
})

test_that("geom_epicurve respects different date resolutions", {
  test_dates <- data.frame(
    date = rep(as.Date("2024-01-01") + 0:30, each = 2)
  )

  # Test different resolutions
  resolutions <- c("day", "week", "month")
  for (res in resolutions) {
    p <- ggplot(test_dates, aes(x = date)) +
      geom_epicurve(date_resolution = res)
    expect_s3_class(p, "ggplot")
  }
})

test_that("geom_epicurve handles NA values correctly", {
  # Test data with NA values
  test_dates <- data.frame(
    date = c(as.Date("2024-01-01") + 0:5, NA, as.Date("2024-01-08") + 0:2),
    cat = c(rep("A", 6), NA, rep("B", 3))
  )

  # Both plots should still render successfully
  expect_no_error({
    p1 <- ggplot(test_dates, aes(x = date, fill = cat)) +
      geom_epicurve(date_resolution = "day", na.rm = FALSE)
    p1
  })
  expect_s3_class(p1, "ggplot")

  p2 <- ggplot(test_dates, aes(x = date, fill = cat)) +
    geom_epicurve(date_resolution = "day", na.rm = TRUE)
  expect_s3_class(p2, "ggplot")
})


test_that("geom_epicurve with stat = 'bin_date'", {
  plot_data_epicurve_imp <- data.frame(
    date = rep(as.Date("2024-01-01") + ((0:300) * 1), times = rpois(301, 0.5))
    # category = rep(c("A", "B"), times = 7)
  )

  expect_no_error({
    ggplot(plot_data_epicurve_imp, aes(x = date, weight = 2)) +
      geom_epicurve(date_resolution = "month", color = "black", just = 0.5, relative.width = 1, stat = "bin_date")
  })

  expect_no_error({
    ggplot(plot_data_epicurve_imp, aes(x = date, weight = 2)) +
      geom_epicurve(color = "black", just = 0.5, relative.width = 1, stat = "bin_date")
  })
})

test_that("scale_y_cases_5er: .auto_pretty", {
  expect_identical(.auto_pretty()(1:100), (0:10) * 10)
  expect_identical(.auto_pretty()(1:200), (0:10) * 20)
  expect_identical(.auto_pretty()(1:120), (0:6) * 20)
  expect_identical(.auto_pretty()(1), (0:1) * 1.0)
  expect_identical(.auto_pretty()(0L:7L), (0:7) * 1.0)
  # Pass ... arguments to pretty
  expect_identical(.auto_pretty(n = 3, min.n = 1)(1:200), (0:4) * 50)
  expect_identical(.auto_pretty(n = 3, min.n = 1, high.u.bias = 10^10)(1:200), (0:2) * 100)
})
