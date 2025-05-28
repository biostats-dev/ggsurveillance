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
    scale_y_cases_5er() +
    theme_mod_legend_position(position.inside = c(0.5, 0.5)) +
    theme_mod_rotate_x_axis_labels_90() +
    theme_mod_remove_panel_grid()

  # Test that the plot is created successfully
  expect_s3_class(p, "ggplot")
  expect_no_error(p)
  vdiffr::expect_doppelganger("1_geom_epicurve_basic_date", p)
})

test_that("geom_epicurve handles date_resolution = NA/NULL", {
  # Test data
  test_dates <- data.frame(
    date = as.Date("2024-01-01") + 0:10,
    cat = c(rep("A", 5), rep("B", 6))
  )

  # Create plot
  p <- ggplot(test_dates, aes(x = date)) +
    geom_vline_year() +
    geom_epicurve(aes(fill = cat), date_resolution = "week") +
    stat_bin_date(aes(y = after_stat(count) * 1.05, label = after_stat(count)),
      geom = "text", date_resolution = "week",
    ) +
    # Test label_skip()
    scale_y_cases_5er(labels = label_skip()) +
    theme_mod_legend_top() +
    theme_mod_remove_legend_title()

  expect_no_error(p)
  vdiffr::expect_doppelganger("2_geom_epicurve_date_resolutionNA", p)
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
    scale_x_cases_5er(n = 10, n.min = 9, u5.bias = 3) +
    theme_mod_legend_left()

  # Test that the plot is created successfully
  expect_s3_class(p, "ggplot")
  expect_no_error(p)
  vdiffr::expect_doppelganger("3_geom_epicurve_flipped_aes", p)

  p1 <- ggplot(test_dates, aes(y = date, fill = cat)) +
    geom_hline_year() +
    geom_epicurve(date_resolution = "day") +
    scale_x_cases_5er(n = 10, n.min = 9, u5.bias = 3) +
    theme_mod_legend_left() +
    coord_flip()

  # Test that the plot is created successfully
  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  vdiffr::expect_doppelganger("3_geom_epicurve_flipped_aes_back", p1)

  p2 <- ggplot(test_dates, aes(y = date, fill = cat)) +
    geom_hline_year() +
    geom_epicurve(date_resolution = "day") +
    scale_x_cases_5er(n = 10, n.min = 9, u5.bias = 3) +
    theme_mod_legend_left() +
    facet_wrap(~cat)

  # Test that the plot is created successfully
  expect_s3_class(p2, "ggplot")
  expect_no_error(p2)
  vdiffr::expect_doppelganger("3_geom_epicurve_facet", p2)
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
  expect_no_error(p)
  vdiffr::expect_doppelganger("4_geom_epicurve_datetime", p)
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
    expect_no_error(p)
    vdiffr::expect_doppelganger(paste0("5_geom_epicurve_res_", res), p)
  }
})

test_that("geom_epicurve handles NA values correctly", {
  # Test data with NA values
  test_dates <- data.frame(
    date = c(as.Date("2024-01-01") + 0:5, NA, as.Date("2024-01-08") + 0:2),
    cat = c(rep("A", 6), NA, rep("B", 3))
  )

  # Both plots should still render successfully
  p1 <- ggplot(test_dates, aes(x = date, fill = cat)) +
    geom_epicurve(date_resolution = "day", na.rm = FALSE) +
    theme_mod_rotate_x_axis_labels_60() +
    theme_mod_remove_minor_grid()
  expect_s3_class(p1, "ggplot")
  expect_no_error(p1)
  expect_warning(vdiffr::expect_doppelganger("6_geom_epicurve_na", p1))

  p2 <- ggplot(test_dates, aes(x = date, fill = cat)) +
    geom_epicurve(date_resolution = "day", na.rm = TRUE)
  expect_s3_class(p2, "ggplot")
  expect_no_error(p2)
  expect_no_warning(vdiffr::expect_doppelganger("6_geom_epicurve_na_TRUE", p2))
})


test_that("geom_epicurve with stat = 'bin_date'", {
  set.seed(123)
  plot_data_epicurve_imp <- data.frame(
    date = rep(as.Date("2024-01-01") + ((0:300) * 1), times = rpois(301, 0.5))
    # category = rep(c("A", "B"), times = 7)
  )

  p1 <- ggplot(plot_data_epicurve_imp, aes(x = date, weight = 2)) +
    geom_epicurve(
      date_resolution = "month", color = "black", just = 0.5,
      relative.width = 1, stat = "bin_date"
    ) +
    theme_mod_rotate_x_axis_labels_45() +
    theme_mod_remove_minor_grid_y()
  expect_no_error(p1)
  expect_no_warning(vdiffr::expect_doppelganger("7_geom_epicurve_bin_date_res", p1))

  p2 <- ggplot(plot_data_epicurve_imp, aes(x = date, weight = 2)) +
    geom_epicurve(color = "black", just = 0.5, relative.width = 1, stat = "bin_date") +
    theme_mod_rotate_x_axis_labels_30() +
    theme_mod_remove_minor_grid_x()
  expect_no_error(p2)
  expect_warning(vdiffr::expect_doppelganger("7_geom_epicurve_bin_date_no_res", p2))
})

test_that("stat_bin_date: test fill_gaps", {
  set.seed(123)
  plot_data_epicurve_imp <- data.frame(
    date = rep(as.Date("2024-11-01") + ((0:300) * 1), times = rpois(301, 0.5))
    # category = rep(c("A", "B"), times = 7)
  )
  
  p1 <- ggplot(plot_data_epicurve_imp, aes(x = date, weight = 2)) +
    stat_bin_date(date_resolution = "week") +
    scale_y_cases_5er()
  expect_no_error(p1)
  vdiffr::expect_doppelganger("8_stat_bin_date_base", p1)
  
  p2 <- ggplot(plot_data_epicurve_imp, aes(x = date, weight = 2)) +
    stat_bin_date(date_resolution = "week", fill_gaps = TRUE) +
    scale_y_cases_5er()
  expect_no_error(p2)
  vdiffr::expect_doppelganger("8_stat_bin_date_fill_gaps", p2)
  
  p3 <- ggplot(plot_data_epicurve_imp, aes(x = date, weight = 2)) +
    stat_bin_date(aes(label = after_stat(count)), date_resolution = "year", geom = "text")
  expect_no_error(p3)
  vdiffr::expect_doppelganger("8_stat_bin_date_year", p3)
  
  p4 <- ggplot(plot_data_epicurve_imp, aes(x = date, weight = 2)) +
    stat_bin_date(aes(label = after_stat(count)), date_resolution = "isoyear", geom = "text")
  expect_no_error(p4)
  vdiffr::expect_doppelganger("8_stat_bin_date_isoyear", p4)
  
  p5 <- ggplot(plot_data_epicurve_imp, aes(x = date, weight = 2)) +
    stat_bin_date(aes(label = after_stat(count)), date_resolution = "epiyear", geom = "text")
  expect_no_error(p5)
  vdiffr::expect_doppelganger("8_stat_bin_date_epiyear", p5)
  
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
