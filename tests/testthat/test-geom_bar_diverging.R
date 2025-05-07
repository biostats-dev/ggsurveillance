test_that("geom_bar_diverging creates basic diverging bar charts", {
  # Test data with even number of factor levels
  df_even <- data.frame(
    name = rep(letters[1:3], each = 4),
    value = factor(rep(c("++", "+", "-", "--"), 3),
      levels = c("++", "+", "-", "--")
    )
  )

  # Create basic horizontal plot
  p1 <- ggplot(df_even, aes(y = name, fill = value)) +
    geom_bar_diverging() +
    stat_diverging() +
    scale_x_continuous_diverging()

  # Test that the plot is created successfully
  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  vdiffr::expect_doppelganger("1_geom_bar_diverging_basic_horizontal", p1)

  # Create basic vertical plot
  p2 <- ggplot(df_even, aes(x = name, fill = value)) +
    geom_bar_diverging() +
    stat_diverging(aes(label = after_stat(fill))) +
    scale_y_continuous_diverging()

  # Test that the plot is created successfully
  expect_s3_class(p2, "ggplot")
  expect_no_error(p2)
  vdiffr::expect_doppelganger("1_geom_bar_diverging_basic_vertical", p2)

  # Test with proportion = TRUE
  p3 <- ggplot(df_even, aes(y = name, fill = value)) +
    geom_bar_diverging(proportion = TRUE) +
    stat_diverging(proportion = TRUE) +
    scale_x_continuous_diverging(labels = scales::label_percent())

  expect_s3_class(p3, "ggplot")
  expect_no_error(p3)
  vdiffr::expect_doppelganger("1_geom_bar_diverging_proportion", p3)
})

test_that("geom_bar_diverging handles odd number of factor levels correctly", {
  # Test data with odd number of factor levels
  df_odd <- data.frame(
    name = rep(letters[1:3], each = 5),
    value = factor(rep(c("++", "+", "+/-", "-", "--"), 3),
      levels = c("++", "+", "+/-", "-", "--")
    )
  )

  # Test with neutral_cat = "odd" (default)
  p1 <- ggplot(df_odd, aes(y = name, fill = value)) +
    geom_bar_diverging()

  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  vdiffr::expect_doppelganger("2_geom_bar_diverging_neutral_odd", p1)

  # Test with neutral_cat = "never"
  p2 <- ggplot(df_odd, aes(y = name, fill = value)) +
    geom_bar_diverging(neutral_cat = "never")

  expect_s3_class(p2, "ggplot")
  expect_no_error(p2)
  vdiffr::expect_doppelganger("2_geom_bar_diverging_neutral_never", p2)
})

test_that("stat_diverging adds totals to diverging bar charts", {
  # Test data
  df <- data.frame(
    name = rep(letters[1:3], each = 4),
    value = factor(rep(c("++", "+", "-", "--"), 3),
      levels = c("++", "+", "-", "--")
    )
  )

  # Labels with totals by direction
  p1 <- ggplot(df, aes(y = name, fill = value)) +
    geom_bar_diverging() +
    stat_diverging(size = 3, totals_by_direction = TRUE, nudge_label_outward = 0.05) +
    scale_x_continuous_diverging(transform = "reverse")

  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  vdiffr::expect_doppelganger("3_stat_diverging_totals", p1)

  # Labels with proportion
  p2 <- ggplot(df, aes(y = name, fill = value)) +
    geom_bar_diverging(proportion = TRUE) +
    stat_diverging(size = 3, proportion = TRUE, totals_by_direction = TRUE, nudge_label_outward = 0.05)

  expect_s3_class(p2, "ggplot")
  expect_no_error(p2)
  vdiffr::expect_doppelganger("3_stat_diverging_proportion", p2)
})

test_that("complex diverging plots combine all components correctly", {
  # Test data with 5 factor levels
  set.seed(123)
  df <- data.frame(
    matrix(sample.int(5, 600, replace = TRUE),
      ncol = 6
    )
  ) |>
    dplyr::mutate_all(~ ordered(., labels = c("++", "+", "+/-", "-", "--"))) |>
    tidyr::pivot_longer(cols = dplyr::everything())

  # Complete diverging bar chart with all components
  p <- ggplot(df, aes(y = name, fill = value)) +
    geom_vline(xintercept = 0) +
    geom_bar_diverging(proportion = TRUE) +
    stat_diverging(
      size = 3,
      proportion = TRUE,
      totals_by_direction = TRUE,
      nudge_label_outward = 0.05
    ) +
    scale_x_continuous_diverging(
      labels = scales::label_percent(),
      n.breaks = 10
    ) +
    theme_classic()

  expect_s3_class(p, "ggplot")
  expect_no_error(p)
  vdiffr::expect_doppelganger("4_complex_diverging_plot", p)
})

test_that("diverging bar charts work with weight aesthetic", {
  # Test data with weights
  df <- data.frame(
    name = rep(letters[1:3], each = 4),
    value = factor(rep(c("++", "+", "-", "--"), 3),
      levels = c("++", "+", "-", "--")
    ),
    weight = rep(c(10, 5, 15, 20), 3)
  )

  p <- ggplot(df, aes(y = name, fill = value, weight = weight)) +
    geom_bar_diverging() +
    stat_diverging(size = 3)

  expect_s3_class(p, "ggplot")
  expect_no_error(p)
  vdiffr::expect_doppelganger("5_geom_bar_diverging_weighted", p)
})

test_that("diverging bar charts handle single category gracefully", {
  # Test data with only one factor level
  df_single <- data.frame(
    name = rep(letters[1:3], each = 1),
    value = factor(rep("++", 3), levels = c("++"))
  )

  # Expect warning but still create plot
  p <- ggplot(df_single, aes(y = name, fill = value)) +
    geom_bar_diverging()

  expect_s3_class(p, "ggplot")
  expect_no_error(p)
  expect_warning(
    vdiffr::expect_doppelganger("6_geom_bar_diverging_single_category", p),
    "diverging_groups should have at least 2 factor levels"
  )
})

test_that("diverging bar charts with continuous y", {
  df <- data.frame(
    name = rep(1:3, each = 4),
    value = factor(rep(c("++", "+", "-", "--"), 3),
                   levels = c("++", "+", "-", "--")
    ),
    weight = rep(c(10, 5, 15, 20), 3)
  )
  
  p <- ggplot(df, aes(y = name, fill = value)) +
    geom_bar_diverging() +
    stat_diverging(size = 3) +
    scale_x_continuous(labels = c(-2:2))
  
  expect_s3_class(p, "ggplot")
  expect_no_error(p)
  vdiffr::expect_doppelganger("7_geom_bar_diverging_continuous", p)
  
  p1 <- ggplot(df, aes(x = name, fill = value)) +
    geom_area_diverging() +
    stat_diverging(size = 3) +
    scale_y_continuous_diverging(labels = c(-2:2))
  
  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  vdiffr::expect_doppelganger("7_geom_area_diverging_1", p1)
  
  p2 <- ggplot(df, aes(x = name, fill = value)) +
    geom_area_diverging() +
    stat_diverging(size = 3) +
    scale_y_continuous_diverging(labels = scales::label_currency())
  
  expect_s3_class(p2, "ggplot")
  expect_no_error(p2)
  vdiffr::expect_doppelganger("7_geom_area_diverging_2", p2)
  
})

test_that("StatDiverging$compute_panel() correctly processes data", {
  # Test data with even number of factor levels
  test_data <- data.frame(
    PANEL = 1,
    group = 1:12,
    x = rep(letters[1:3], each = 4),
    weight = 1,
    diverging_groups = factor(rep(c("++", "+", "-", "--"), 3), levels = c("++", "+", "-", "--"))
  )
  
  # Test with basic parameters
  result <- expect_no_error(
    StatDiverging$compute_panel(
      data = test_data,
      scales = list(),
      flipped_aes = FALSE,
      stacked = TRUE,
      width = 0.9,
      neutral_cat = "odd",
      proportion = FALSE,
      totals_by_direction = FALSE,
      nudge_label_outward = 0
    )
  )
  
  # Check structure of result
  expect_s3_class(result, "data.frame")
  expect_true(all(c("x", "diverging_groups", "count", "ymin", "ymax", "sign", "y", "total", 
                    "total_neg", "total_pos", "prop", "prop_neg", "prop_pos") %in% names(result)))
  
  # Check correct calculations
  expect_equal(result$total, rep(4, 12))
  expect_equal(sum(result$count), nrow(test_data))  # Total count matches input rows
  
  # Test that signs are correctly assigned (++ and + should be positive, - and -- negative)
  negative_groups <- result$diverging_groups %in% c("++", "+")
  positive_groups <- result$diverging_groups %in% c("-", "--")
  expect_true(all(result$sign[positive_groups] > 0))
  expect_true(all(result$sign[negative_groups] < 0))
  
  # Verify stacking behavior
  expect_true(all(result$ymax[result$diverging_groups == "++"] < 0))
  expect_true(all(result$ymin[result$diverging_groups == "--"] > 0))
  
  # Test with proportion = TRUE
  result_prop <- expect_no_error(
    StatDiverging$compute_panel(
      data = test_data,
      scales = list(),
      flipped_aes = FALSE,
      stacked = TRUE,
      width = 0.9,
      neutral_cat = "odd", 
      proportion = TRUE,
      totals_by_direction = FALSE,
      nudge_label_outward = 0
    )
  )
  
  # Check proportion calculations
  expect_true(all(result_prop$ymin >= -1 & result_prop$ymin <= 1))
  expect_true(all(result_prop$ymax >= -1 & result_prop$ymax <= 1))
  expect_equal(sum(result_prop$prop[result_prop$x == "a"]), 1)
  
  # Test with totals_by_direction = TRUE
  result_totals <- expect_no_error(
    StatDiverging$compute_panel(
      data = test_data,
      scales = list(),
      flipped_aes = FALSE,
      stacked = TRUE,
      width = 0.9,
      neutral_cat = "odd",
      proportion = FALSE,
      totals_by_direction = TRUE,
      nudge_label_outward = 0
    )
  )
  
  # Check totals calculations - should have 2 rows per x value (positive and negative)
  expect_equal(nrow(result_totals), 6)  # 3 x values × 2 directions
  expect_equal(sum(result_totals$count), sum(test_data$weight))
  expect_true(all(result_totals$sign %in% c(-1, 1)))
})

test_that("StatDiverging$compute_panel() handles odd factor levels correctly", {
  # Test data with odd number of factor levels
  test_data <- data.frame(
    PANEL = 1,
    group = 1:15,
    x = rep(letters[1:3], each = 5),
    weight = 1,
    diverging_groups = factor(rep(c("++", "+", "+/-", "-", "--"), 3), levels = c("++", "+", "+/-", "-", "--"))
  )
  
  # Test with neutral_cat = "odd" (default)
  result_odd <- expect_no_error(
    StatDiverging$compute_panel(
      data = test_data,
      scales = list(),
      flipped_aes = FALSE,
      stacked = TRUE,
      width = 0.9,
      neutral_cat = "odd",
      proportion = FALSE,
      totals_by_direction = FALSE,
      nudge_label_outward = 0
    )
  )
  
  # The middle value "+/-" should be split between positive and negative
  middle_values <- result_odd[result_odd$diverging_groups == "+/-", ]
  expect_equal(nrow(middle_values), 3)  # One for each name
  expect_true(all(middle_values$ymin < 0))
  expect_true(all(middle_values$ymax > 0))
  
  # Test with neutral_cat = "never"
  result_never <- expect_no_error(
    StatDiverging$compute_panel(
      data = test_data,
      scales = list(),
      flipped_aes = FALSE,
      stacked = TRUE,
      width = 0.9,
      neutral_cat = "never",
      proportion = FALSE,
      totals_by_direction = FALSE,
      nudge_label_outward = 0
    )
  )
  
  # Check that middle values are treated as positive when neutral_cat = "never"
  middle_values_never <- result_never[result_never$diverging_groups == "+/-", ]
  expect_equal(nrow(middle_values_never), 3)
  expect_true(all(middle_values_never$ymin >= 0))
  expect_true(all(middle_values_never$ymax > 0))
})

test_that("StatDiverging$compute_panel() handles nudge_label_outward correctly", {
  # Test data
  test_data <- data.frame(
    PANEL = 1,
    group = 1:12,
    x = rep(letters[1:3], each = 4),
    weight = 1,
    diverging_groups = factor(rep(c("++", "+", "-", "--"), 3), levels = c("++", "+", "-", "--"))
  )
  
  # Test with nudge_label_outward = 0
  result_no_nudge <- expect_no_error(
    StatDiverging$compute_panel(
      data = test_data,
      scales = list(),
      flipped_aes = FALSE,
      stacked = TRUE, 
      width = 0.9,
      neutral_cat = "odd",
      proportion = FALSE,
      totals_by_direction = FALSE,
      nudge_label_outward = 0
    )
  )
  
  # Test with nudge_label_outward = 0.05
  result_nudge <- expect_no_error(
    StatDiverging$compute_panel(
      data = test_data,
      scales = list(),
      flipped_aes = FALSE,
      stacked = TRUE, 
      width = 0.9,
      neutral_cat = "odd",
      proportion = FALSE,
      totals_by_direction = FALSE,
      nudge_label_outward = 0.05
    )
  )
  
  # Verify that the y-coordinates have been nudged outward
  expect_true(all(result_nudge$y[result_nudge$sign > 0] > result_no_nudge$y[result_no_nudge$sign > 0]))
  expect_true(all(result_nudge$y[result_nudge$sign < 0] < result_no_nudge$y[result_no_nudge$sign < 0]))
})

test_that("StatDiverging$compute_panel() works with weight aesthetic", {
  # Test data with weights
  test_data <- data.frame(
    PANEL = 1,
    group = 1:12,
    x = rep(letters[1:3], each = 4),
    weight = rep(c(10, 5, 15, 20), 3),
    diverging_groups = factor(rep(c("++", "+", "-", "--"), 3), levels = c("++", "+", "-", "--"))
  )
  
  result <- expect_no_error(
    StatDiverging$compute_panel(
      data = test_data,
      scales = list(),
      flipped_aes = FALSE,
      stacked = TRUE,
      width = 0.9,
      neutral_cat = "odd",
      proportion = FALSE,
      totals_by_direction = FALSE,
      nudge_label_outward = 0
    )
  )
  
  # Verify weights were applied correctly
  expect_equal(sum(result$count), sum(test_data$weight))
  expect_equal(result$total, rep(50, 12))  # 10+5+15+20=50 for each name
  
  # Check specific counts match weights
  expect_equal(result$count[result$diverging_groups == "++" & result$x == "a"], 10)
  expect_equal(result$count[result$diverging_groups == "+" & result$x == "a"], 5)
  expect_equal(result$count[result$diverging_groups == "-" & result$x == "a"], 15)
  expect_equal(result$count[result$diverging_groups == "--" & result$x == "a"], 20)
})

test_that("StatDiverging$compute_panel() handles empty data correctly", {
  # Test with empty data
  empty_data <- data.frame(
    x = character(),
    diverging_groups = factor(levels = c("++", "+", "-", "--")),
    PANEL = numeric(),
    group = numeric()
  )
  
  result <- StatDiverging$compute_panel(
    data = empty_data,
    scales = list(),
    flipped_aes = FALSE,
    stacked = TRUE,
    width = 0.9,
    neutral_cat = "odd",
    proportion = FALSE,
    totals_by_direction = FALSE,
    nudge_label_outward = 0
  )
  
  # Check that an empty data frame is returned
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})