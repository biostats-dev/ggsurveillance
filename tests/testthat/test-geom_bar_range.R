test_that("geom_col_range creates basic ranged bar charts", {
  # Test data
  df <- data.frame(
    x = 1:3,
    ymin = -1:-3,
    ymax = 1:3
  )

  # Create basic plot
  p <- ggplot(df, aes(x = x, ymin = ymin, ymax = ymax)) +
    geom_col_range()

  # Test that the plot is created successfully
  expect_s3_class(p, "ggplot")
  expect_no_error(p)
  vdiffr::expect_doppelganger("1_geom_col_range_basic", p)

  p1 <- ggplot(df, aes(y = x, xmin = ymin, xmax = ymax)) +
    geom_col_range()

  # Test that the plot is created successfully
  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  vdiffr::expect_doppelganger("1_geom_col_range_flip", p1)
})

test_that("geom_col_range works with different position parameters", {
  # Test data
  df <- data.frame(
    x = rep(1:3, each = 2),
    ymin = -c(1, 2, 3, 4, 5, 6),
    ymax = c(2, 1, 4, 3, 6, 5),
    group = rep(c("A", "B"), 3)
  )

  # Create plot with position = "dodge"
  p <- ggplot(df, aes(x = x, ymin = ymin, ymax = ymax, fill = group)) +
    geom_col_range(position = "dodge")

  # Test that the plot is created successfully
  expect_s3_class(p, "ggplot")
  expect_no_error(p)
  vdiffr::expect_doppelganger("2_geom_col_range_position_dodge", p)
})

test_that("geom_col_range respects the 'just' parameter", {
  # Test data
  df <- data.frame(
    x = 1:3,
    ymin = -1:-3,
    ymax = 1:3
  )

  # Create plots with different 'just' values
  p1 <- ggplot(df, aes(x = x, ymin = ymin, ymax = ymax)) +
    geom_col_range(just = 0) # Left-aligned

  p2 <- ggplot(df, aes(x = x, ymin = ymin, ymax = ymax)) +
    geom_col_range(just = 1) # Right-aligned

  # Test that the plots are created successfully
  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  vdiffr::expect_doppelganger("3_geom_col_range_just_0", p1)

  expect_s3_class(p2, "ggplot")
  expect_no_error(p2)
  vdiffr::expect_doppelganger("3_geom_col_range_just_1", p2)
})
