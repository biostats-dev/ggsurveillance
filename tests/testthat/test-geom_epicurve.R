test_that("geom_epicurve handles basic date inputs", {
  # Test data
  test_dates <- data.frame(
    date = as.Date("2024-01-01") + 0:10,
    cat = c(rep("A", 5), rep("B", 6))
  )

  # Create plot
  p <- ggplot(test_dates, aes(x = date, fill = cat)) +
    geom_epicurve(date.resolution = "day")

  # Test that the plot is created successfully
  expect_s3_class(p, "ggplot")
})

test_that("geom_epicurve handles datetime data", {
  test_datetime <- data.frame(
    datetime = as.POSIXct("2024-01-01") + seq(0, 86400 * 20, by = 86400)
  )

  p <- ggplot(test_datetime, aes(x = datetime)) +
    geom_epicurve(date.resolution = "day")

  expect_s3_class(p, "ggplot")
})

test_that("geom_epicurve respects different date resolutions", {
  test_dates <- data.frame(
    date = rep(as.Date("2024-01-01") + 0:30, each = 2)
  )

  # Test different resolutions
  resolutions <- c("day", "week", "month")
  for (res in resolutions) {
    p <- ggplot(test_dates, aes(x = date)) +
      geom_epicurve(date.resolution = res)
    expect_s3_class(p, "ggplot")
  }
})
