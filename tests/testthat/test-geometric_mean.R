test_that("geometric_mean works with simple positive input and defaults", {
  x <- c(1, 2, 3, 4, 5)
  # Manually compute expected geometric mean
  expected <- exp(mean(log(x)))
  expect_identical(geometric_mean(c(1)), 1)
  expect_identical(geometric_mean(c(1, 4)), 2)
  expect_identical(geometric_mean(x), expected)
})

test_that("geometric_mean returns NA for non-positive values when na.rm = FALSE", {
  x <- c(0, 1, 2, 3)
  expect_warning(
    gm <- geometric_mean(x, na.rm = FALSE),
    regexp = "Zero or negative values are treated as NA" # or partial match
  )
  expect_true(is.na(gm))
})

test_that("geometric_mean removes non-positive values when na.rm = TRUE", {
  x <- c(-1, 0, 1, 4)
  # Only 1 and 4 remain
  gm <- geometric_mean(x, na.rm = TRUE)
  expect_identical(gm, 2)
})

test_that("geometric_mean handles zero replacement with replace = 'zero'", {
  x <- c(0, 1, 2)
  # Replace zero with 0.5, so x becomes c(0.5, 1, 2)
  # geometric mean = exp(mean(log(c(0.5, 1, 2))))
  expected <- exp(mean(log(c(0.5, 1, 2))))

  expect_message(
    gm <- geometric_mean(x, replace_value = 0.5, replace = "zero"),
    regexp = "value.*were substituted with 0.5"
  )
  expect_identical(gm, expected)
})

test_that("geometric_mean handles 'all' replacement below threshold", {
  x <- c(0.1, 0.2, -1, 1, 5)
  # With replace = 'all' and replace_value = 0.5, we replace anything < 0.5.
  # So x becomes c(0.5, 0.5, 0.7, 1, 5)
  expected <- exp(mean(log(c(0.5, 0.5, 0.5, 1, 5))))
  expect_message(
    gm <- geometric_mean(x, replace_value = 0.5, replace = "all"),
    regexp = "3 values were substituted with 0.5"
  )
  expect_identical(gm, expected)
})

test_that("geometric_mean handles non-positive replacement with replace = 'non-positive'", {
  x <- c(-1, 0, 0.1, 1, 2)
  # Replace all non-positives with 0.5
  expected <- exp(mean(log(c(0.5, 0.5, 0.1, 1, 2))))
  expect_message(
    gm <- geometric_mean(x, replace_value = 0.5, replace = "non-positive"),
    regexp = "2 values were substituted with 0.5"
  )
  expect_identical(gm, expected)
})

test_that("geometric_mean returns NA if all values are removed (na.rm = TRUE)", {
  x <- c(-1, 0, -5)
  # After removing non-positive, the vector is empty
  expect_identical(geometric_mean(x, na.rm = TRUE), NA_real_)
})

test_that("geometric_mean returns NA if input is not numeric or complex", {
  x <- c("a", "b", "c")
  # Expect a warning and an NA return
  expect_warning(gm <- geometric_mean(x))
  expect_true(is.na(gm))
})

test_that("geometric_mean handles message counting replacements correctly", {
  x <- c(0, 0, 1, 2, 3)
  # Expect 2 replacements
  expect_message(
    gm <- geometric_mean(x, replace_value = 0.1, replace = "zero"),
    "2 value"
  )
  # Now x effectively becomes c(0.1, 0.1, 1, 2, 3).
  expected <- exp(mean(log(c(0.1, 0.1, 1, 2, 3))))
  expect_equal(gm, expected)
})
