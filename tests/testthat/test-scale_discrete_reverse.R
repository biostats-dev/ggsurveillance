test_that("Test scale_discrete_reverse", {
  library(ggplot2)

  df <- data.frame(
    category = factor(c("A", "B", "C", "D")),
    value = c(10, 5, 8, 3)
  )

  expect_no_error(ggplot(df, aes(x = value, y = category)) +
    geom_col() +
    scale_y_discrete_reverse())

  expect_no_error(ggplot(df, aes(x = value, y = category)) +
    geom_col() +
    scale_y_discrete_reverse(limits = c("A", "B", "C", "D")))

  expect_no_error(ggplot(df, aes(y = value, x = category)) +
    geom_col() +
    scale_x_discrete_reverse())

  # Double reverse
  expect_no_error(ggplot(df, aes(y = value, x = category)) +
    geom_col() +
    scale_x_discrete_reverse(limits = c("D", "C", "B", "A")))
})