test_that("basic functionality works", {
  # Test default settings
  expect_identical(
    create_agegroups(1:10),
    c("0-4", "0-4", "0-4", "0-4", "5-9", "5-9", "5-9", "5-9", "5-9", "10-14")
  )

  # Test with return_factor = TRUE
  expect_s3_class(
    create_agegroups(1:10, return_factor = TRUE),
    "factor"
  )
})

test_that("custom breaks work with sorting and NA removal", {
  expect_identical(
    create_agegroups(1:5, age_breaks = c(4, NA, 2)),
    c("0-1", "2-3", "2-3", "4+", "4+")
  )
})

test_that("different formatting options work", {
  # Test upper bound formatting
  expect_identical(
    create_agegroups(1:5,
      age_breaks = c(2, 4),
      breaks_as_lower_bound = FALSE,
      first_group_format = "≤{x}",
      interval_format = "{x} to {y}",
      last_group_format = "≥{x}"
    ),
    c("≤2", "≤2", "3 to 4", "3 to 4", "≥5")
  )

  # Test number padding
  expect_identical(
    create_agegroups(c(1:5, 10),
      age_breaks = c(2, 4, 10),
      pad_numbers = 2
    ),
    c("0-01", "02-03", "02-03", "04-09", "04-09", "10+")
  )
})

test_that("single year group collapsing works", {
  expect_identical(
    create_agegroups(1:5,
      age_breaks = 1:5,
      collapse_single_year_groups = TRUE
    ),
    c("1", "2", "3", "4", "5+")
  )
})

test_that("NA handling works", {
  # Test default NA handling
  expect_true(is.na(create_agegroups(c(1, NA, 3))[2]))

  # Test custom NA label
  expect_identical(
    create_agegroups(c(1, NA, 3), na_label = "Missing"),
    c("0-4", "Missing", "0-4")
  )
})

test_that("input validation works", {
  # Test non-numeric breaks
  expect_error(
    create_agegroups(1:5, age_breaks = c("1", "2"))
  )
})
