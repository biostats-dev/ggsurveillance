test_that("label_skip basic functionality works", {
  # Test default parameters (n=2, start="left")
  skip_labels <- label_skip()
  expect_identical(skip_labels(1:10), c("1", "", "3", "", "5", "", "7", "", "9", ""))

  # Test with different n values
  skip_labels_3 <- label_skip(n = 3)
  expect_identical(skip_labels_3(1:10), c("1", "", "", "4", "", "", "7", "", "", "10"))

  # Test with n=1 (no skipping)
  skip_labels_1 <- label_skip(n = 1)
  expect_identical(skip_labels_1(1:5), as.character(1:5))
})

test_that("label_skip handles start parameter correctly", {
  # Test with start="right"
  skip_labels_right <- label_skip(n = 2, start = "right")
  expect_identical(skip_labels_right(1:10), c("", "2", "", "4", "", "6", "", "8", "", "10"))

  # Test with integer start position
  skip_labels_pos2 <- label_skip(n = 3, start = 2)
  expect_identical(skip_labels_pos2(1:10), c("", "2", "", "", "5", "", "", "8", "", ""))

  # Test with integer start wrapping around n
  skip_labels_pos5 <- label_skip(n = 3, start = 5)
  expect_identical(skip_labels_pos5(1:10), c("", "2", "", "", "5", "", "", "8", "", ""))
})

test_that("label_skip handles NA values correctly", {
  # Test with NA values in input
  test_labels <- c(NA, 1, 2, 3, 4, 5, NA)
  skip_labels <- label_skip(n = 2)
  result <- skip_labels(test_labels)
  expect_identical(result, c(NA, "1", "", "3", "", "5", NA))
})

test_that("label_skip works with custom labeller", {
  # Test with a simple labeller function
  add_prefix <- function(x) paste0("Value: ", x)
  skip_with_prefix <- label_skip(n = 2, labeller = add_prefix)
  expect_identical(
    skip_with_prefix(1:5),
    c("Value: 1", "", "Value: 3", "", "Value: 5"),
    ignore_attr = TRUE
  )

  # Test with scales date labeller
  test_dates <- as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01"))
  date_labeller <- label_skip(n = 2, labeller = label_date("%b %Y"))
  expect_identical(
    date_labeller(test_dates),
    c("Jan 2023", "", "Mar 2023", ""),
    ignore_attr = TRUE
  )
})

test_that("label_skip throws appropriate errors", {
  # Test with invalid n
  expect_error(label_skip(n = 0), "'n' must be a positive integer")
  expect_error(label_skip(n = -1), "'n' must be a positive integer")

  # Test with invalid start
  expect_error(label_skip(start = TRUE), "'start' must be either 'left', 'right', or an integer")

  # Test with invalid labeller
  expect_error(label_skip(labeller = "not_a_function"), "not found")
})
