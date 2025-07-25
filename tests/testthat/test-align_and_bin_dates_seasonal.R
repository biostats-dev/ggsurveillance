test_that("align_and_bin_dates_seasonal correctly aggregates numeric values", {
  # Create test data with counts
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-03-01"), by = "week")
  df <- data.frame(
    date = dates,
    cases = sample(1:100, length(dates), replace = TRUE)
  )

  # Test aggregation
  result <- align_and_bin_dates_seasonal(
    df,
    n = cases,
    dates_from = date,
    date_resolution = "month",
    population = 1000
  )

  # Check structure
  expect_true("n" %in% names(result))
  expect_true(all(result$n >= 0))

  # Check that counts are properly summed
  # Compare total cases before and after binning
  expect_identical(
    sum(df$cases),
    sum(result$n)
  )
  expect_equal(
    sum(df$cases) / 1000,
    sum(result$incidence)
  )

  # Test datetime / POSIXct
  dates <- seq(as_datetime("2023-01-01"), as_datetime("2024-03-01"), by = "week")
  df2 <- data.frame(
    date = dates,
    cases = df$cases # Use previous case numbers
  )

  # Test aggregation
  result <- align_and_bin_dates_seasonal(
    df2,
    n = cases,
    dates_from = date,
    date_resolution = "month"
  )

  # Compare total cases
  expect_identical(
    sum(df$cases),
    sum(result$n)
  )

  # Test non date column
  expect_error(
    df |>
      align_dates_seasonal(
        dates_from = region,
        date_resolution = "month"
      )
  )

  # Test non existing column
  expect_error(df |>
    align_dates_seasonal(
      dates_from = test,
      date_resolution = "month"
    ))
})

test_that("align_and_bin_dates_seasonal handles quoted column names", {
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-03-01"), by = "week")
  df <- data.frame(
    observation_date = dates,
    case_count = sample(1:100, length(dates), replace = TRUE)
  )

  # Test with quoted column names
  expect_no_error(
    align_and_bin_dates_seasonal(
      df,
      n = "case_count",
      dates_from = "observation_date",
      date_resolution = "week"
    )
  )
})

test_that("align_and_bin_dates_seasonal maintains grouping variables", {
  # Create test data with multiple groups
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-03-01"), by = "week")
  df <- data.frame(
    date = rep(dates, each = 2),
    region = rep(c("A", "B"), length(dates)),
    cases = sample(1:100, length(dates) * 2, replace = TRUE)
  )

  # Group by region before binning
  grouped_result <- df |>
    dplyr::group_by(region) |>
    align_and_bin_dates_seasonal(
      n = cases,
      dates_from = date,
      date_resolution = "month"
    )

  # Check that grouping is maintained
  expect_true("region" %in% names(grouped_result))
  expect_length(unique(grouped_result$region), 2)

  # Check that aggregation is done within groups
  counts_by_region <- tapply(df$cases, df$region, sum)
  result_by_region <- tapply(grouped_result$n, grouped_result$region, sum)
  expect_identical(
    as.numeric(counts_by_region),
    as.numeric(result_by_region)
  )

  # Test grouping by date column
  expect_warning(
    df |>
      dplyr::group_by(date) |>
      align_and_bin_dates_seasonal(
        n = cases,
        dates_from = date,
        date_resolution = "month"
      )
  )
})

test_that("align_and_bin_dates_seasonal handles default n = 1", {
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-03-01"), by = "week")
  df <- data.frame(date = dates)

  result <- align_and_bin_dates_seasonal(
    df,
    dates_from = date,
    date_resolution = "month"
  )

  # Check that n represents counts of dates
  expect_true(all(result$n >= 1))
  expect_identical(
    as.numeric(nrow(df)),
    sum(result$n)
  )
})

test_that("align_and_bin_dates_seasonal handles NA values correctly", {
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-03-01"), by = "week")
  df <- data.frame(
    date = dates,
    cases = sample(1:100, length(dates), replace = TRUE)
  )
  # Insert some NAs
  df$cases[sample(1:nrow(df), 5)] <- NA

  result <- align_and_bin_dates_seasonal(
    df,
    n = cases,
    dates_from = date,
    date_resolution = "month"
  )

  # Check that NAs are handled (summed) correctly
  expect_identical(
    sum(df$cases, na.rm = TRUE),
    sum(result$n, na.rm = TRUE)
  )
})

test_that("align_and_bin_dates_seasonal preserves season information", {
  dates <- seq(as.Date("2023-01-01"), as.Date("2025-03-01"), by = "week")
  df <- data.frame(
    date = dates,
    cases = sample(1:100, length(dates), replace = TRUE)
  )
  for (date_res in c("week", "epiweek", "month", "day")) {
    result <- align_and_bin_dates_seasonal(
      df,
      n = cases,
      dates_from = date,
      date_resolution = date_res
    )

    # Check that season columns exist and are properly formatted
    expect_true("season" %in% names(result), label = date_res)
    expect_true("current_season" %in% names(result), label = date_res)
    expect_match(result$season, "\\d{4}/\\d{2}", label = date_res)
    expect_type(result$current_season, "logical")
  }

  for (date_res in c("week", "epiweek", "month", "day")) {
    result <- align_and_bin_dates_seasonal(
      df,
      n = cases,
      start = 1,
      dates_from = date,
      date_resolution = date_res
    )

    expect_true(all(result$season == result$year), label = date_res)
  }
})

test_that("align_and_bin_dates_seasonal handles quoted population column", {
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-03-01"), by = "week")
  df <- data.frame(
    observation_date = dates,
    case_count = sample(1:100, length(dates), replace = TRUE),
    pop_size = rep(10000, length(dates))
  )

  # Test with quoted column names
  result <- align_and_bin_dates_seasonal(
    df,
    n = "case_count",
    dates_from = "observation_date",
    population = "pop_size",
    date_resolution = "week"
  )

  expect_true("incidence" %in% names(result))
  expect_identical(
    result$incidence,
    result$n / 10000
  )
})

test_that("align_and_bin_dates_seasonal handles varying population sizes", {
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-03-01"), by = "week")
  df <- data.frame(
    date = rep(dates, each = 2),
    region = rep(c("A", "B"), length(dates)),
    cases = sample(1:100, length(dates) * 2, replace = TRUE),
    population = rep(c(10000, 20000), length(dates))
  )

  result <- df |>
    dplyr::group_by(region) |>
    align_and_bin_dates_seasonal(
      n = cases,
      dates_from = date,
      population = population,
      date_resolution = "month"
    )

  # Check incidence calculations per region
  region_a <- subset(result, region == "A")
  region_b <- subset(result, region == "B")

  expect_equal(region_a$incidence, region_a$n / 10000)
  expect_equal(region_b$incidence, region_b$n / 20000)
})

test_that("align_and_bin_dates_seasonal maintains correct incidence with grouping", {
  # Create test data with multiple groups and different populations
  dates <- seq(as.Date("2023-01-01"), as.Date("2024-03-01"), by = "week")
  df <- data.frame(
    date = rep(dates, each = 2),
    age_group = rep(c("0-17", "18+"), length(dates)),
    cases = sample(1:100, length(dates) * 2, replace = TRUE),
    population = rep(c(5000, 15000), length(dates))
  )

  result <- df |>
    dplyr::group_by(age_group) |>
    align_and_bin_dates_seasonal(
      n = cases,
      dates_from = date,
      population = population,
      date_resolution = "month"
    )

  # Verify that incidence is calculated correctly within groups
  expect_true(all(
    tapply(result$incidence, result$age_group, function(x) all(x >= 0))
  ))

  # Check that total incidence makes sense for each group
  group_incidences <- split(result$incidence, result$age_group)
  expect_true(
    mean(group_incidences[["0-17"]]) != mean(group_incidences[["18+"]])
  )
})

test_that("align_and_bin_dates_seasonal fills gaps correctly", {
  dates <- seq(as.Date("2023-01-01"), as.Date("2023-03-28"), by = "week")
  df <- data.frame(
    date = rep(dates, each = 2),
    region = rep(c("A", "B"), length(dates)),
    cases = rep(c(1, 2), length(dates))
  )

  result <- df |>
    filter(lubridate::month(date) != 2) |>
    dplyr::group_by(region) |>
    align_and_bin_dates_seasonal(
      n = cases,
      dates_from = date,
      fill_gaps = TRUE,
      date_resolution = "month"
    )

  expect_identical(result$n, c(5, 0, 4, 10, 0, 8))

  # Check date coercion (2022-W01) with fill gaps
  influenza_germany |>
    filter(AgeGroup == "00+", Cases > 100) |>
    align_and_bin_dates_seasonal(
      dates_from = ReportingWeek,
      n = Cases,
      date_resolution = "isoweek",
      fill_gaps = TRUE,
      start = 28
    ) -> df_flu_aligned

  expect_identical(nrow(df_flu_aligned), 263L)

  set.seed(130)
  df_align_fill_gaps <- data.frame(
    date = rep(as.Date("2024-01-01") + (0:30), times = rpois(31, 1.5))
    # category = rep(c("A", "B"), times = 7)
  )

  result <- df_align_fill_gaps |>
    align_and_bin_dates_seasonal(dates_from = date, date_resolution = "day")

  expect_identical(nrow(result), 24L)
  expect_identical(all(result$n > 0), TRUE)

  result2 <- df_align_fill_gaps |>
    align_and_bin_dates_seasonal(dates_from = date, date_resolution = "day", fill_gaps = TRUE)

  expect_identical(nrow(result2), 31L)
  expect_identical(all(result2$n > 0), FALSE)

  set.seed(123)
  df_align_fill_gaps_group <- data.frame(
    date = rep(as.Date("2024-01-01") + (0:30), times = rpois(31, 2))
  ) |> dplyr::mutate(category = rbinom(n(), 1, 0.5))

  result3 <- df_align_fill_gaps_group |>
    group_by(category) |>
    align_and_bin_dates_seasonal(dates_from = date, date_resolution = "day", fill_gaps = TRUE)

  expect_identical(nrow(result3), 2L * 31L)
  expect_identical(unique(result3$category), c(0L, 1L))
})
