test_that("Test scale_discrete_reverse", {
  df <- data.frame(
    category = factor(c("A", "B", "C", "D")),
    value = c(10, 5, 8, 3)
  )

  vdiffr::expect_doppelganger(
    "1_scale_reverse",
    ggplot(df, aes(x = value, y = category)) +
      geom_col() +
      scale_y_discrete_reverse()
  )

  vdiffr::expect_doppelganger(
    "2_scale_reverse",
    ggplot(df, aes(x = value, y = category)) +
      geom_col() +
      scale_y_discrete_reverse(limits = c("A", "B", "C", "D"))
  )

  vdiffr::expect_doppelganger(
    "3_scale_reverse",
    ggplot(df, aes(y = value, x = category)) +
      geom_col() +
      scale_x_discrete_reverse()
  )

  # Double reverse
  vdiffr::expect_doppelganger(
    "4_scale_reverse",
    ggplot(df, aes(y = value, x = category)) +
      geom_col() +
      scale_x_discrete_reverse(limits = c("D", "C", "B", "A"))
  )
})
