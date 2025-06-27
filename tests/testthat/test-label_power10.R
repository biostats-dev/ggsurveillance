test_that("label_power10 basic functionality works", {
  # Test with default parameters
  labeller <- label_power10()

  # Test basic scientific notation
  result <- labeller(100000)
  expect_true(is.expression(result))
  expect_identical(length(result), 1L)

  # Test that it generates valid expressions
  expect_no_error(eval(result))

  # Test specific result - 100000 should be 1 %*% 10^5
  result_text <- deparse(result[[1]])
  expect_identical(result_text, "1 %*% 10^5")
})

test_that("label_power10 handles different magnitudes correctly", {
  labeller <- label_power10()

  # Test different powers of 10 as vector
  values <- c(1, 10, 100, 1000, 10000)
  result <- labeller(values)

  expect_true(is.expression(result))
  expect_identical(length(result), length(values))

  # Test specific values
  text_results <- sapply(result, deparse)
  expect_identical(text_results[1], "1") # 1 has exponent 0
  expect_identical(text_results[2], "1 %*% 10") # 10 = 1 x 10^1 -> 1 %*% 10
  expect_identical(text_results[3], "1 %*% 10^2") # 100 = 1 x 10^2
  expect_identical(text_results[4], "1 %*% 10^3") # 1000 = 1 x 10^3
  expect_identical(text_results[5], "1 %*% 10^4") # 10000 = 1 x 10^4
})

test_that("label_power10 magnitude_only parameter works correctly", {
  # Test with same values using both settings
  values <- c(250000, 350000)

  # Test with magnitude_only = FALSE (default)
  labeller_default <- label_power10(magnitude_only = FALSE)
  result_default <- labeller_default(values)

  # Test with magnitude_only = TRUE
  labeller_magnitude <- label_power10(magnitude_only = TRUE)
  result_magnitude <- labeller_magnitude(values)

  expect_true(is.expression(result_default))
  expect_true(is.expression(result_magnitude))

  # Convert to text to compare
  text_default <- sapply(result_default, deparse)
  text_magnitude <- sapply(result_magnitude, deparse)

  # magnitude_only should show only the power part
  expect_identical(text_magnitude[1], "10^5")
  expect_identical(text_magnitude[2], "10^5")
  expect_identical(text_default[1], "2.5 %*% 10^5") # Should contain mantissa
  expect_identical(text_default[2], "3.5 %*% 10^5") # Should contain mantissa
})

test_that("label_power10 magnitude_only works for different values in same magnitude", {
  labeller <- label_power10(magnitude_only = TRUE, digits = 6)

  # All these should show as 10^5
  values <- c(100000, 250000, 999999)
  result <- labeller(values)

  text_results <- sapply(result, deparse)

  # All should be identical when showing magnitude only
  expect_identical(text_results[1], "10^5")
  expect_identical(text_results[2], "10^5")
  expect_identical(text_results[3], "10^5")

  # All should be identical
  expect_true(all(text_results == "10^5"))
})

test_that("label_power10 handles exponent = 1 case with magnitude_only", {
  labeller <- label_power10(magnitude_only = TRUE)

  # Test values that result in 10^1 - should show as just "10"
  values <- c(10, 15, 19)
  result <- labeller(values)
  text_results <- sapply(result, deparse)

  # All should show "10", not "10^1"
  expect_true(all(text_results == "10"))
})

test_that("label_power10 handles decimal marks correctly", {
  # Test with comma as decimal mark
  labeller_comma <- label_power10(decimal.mark = ",")
  labeller_dot <- label_power10(decimal.mark = ".")

  values <- c(1.5, 2.5)
  result_comma <- labeller_comma(values)
  result_dot <- labeller_dot(values)

  expect_true(is.expression(result_comma))
  expect_true(is.expression(result_dot))

  # Results should be different due to decimal mark handling
  text_comma <- sapply(result_comma, deparse)
  text_dot <- sapply(result_dot, deparse)

  # Dot version should contain the decimal directly
  expect_identical(text_dot[1], "1.5")
  expect_identical(text_dot[2], "2.5")
  # Comma version should contain paste() for escaped comma
  expect_identical(text_comma[1], 'paste("1,5")')
  expect_identical(text_comma[2], 'paste("2,5")')
})

test_that("label_power10 handles prefix and suffix correctly", {
  # Test with prefix and suffix
  labeller_ps <- label_power10(prefix = "Value: ", suffix = " units")
  values <- c(1000, 2000)
  result <- labeller_ps(values)

  expect_true(is.expression(result))
  text_results <- sapply(result, deparse)

  # Should contain paste() function when prefix/suffix are used
  expect_identical(text_results[1], 'paste("Value: ", 1 %*% 10^3, " units")')
  expect_identical(text_results[2], 'paste("Value: ", 2 %*% 10^3, " units")')
})

test_that("label_power10 handles digits parameter correctly", {
  # Test with different digits settings
  values <- c(12345, 23456)

  labeller_3 <- label_power10(digits = 3)
  labeller_1 <- label_power10(digits = 1)

  result_3 <- labeller_3(values)
  result_1 <- labeller_1(values)

  expect_true(is.expression(result_3))
  expect_true(is.expression(result_1))

  # Results should be different due to different precision
  text_3 <- sapply(result_3, deparse)
  text_1 <- sapply(result_1, deparse)

  # digits=3 should show more precision than digits=1
  expect_identical(text_3[1], "1.23 %*% 10^4") # Should be 1.23 x 10^4
  expect_identical(text_1[2], "2 %*% 10^4") # Should be 2 x 10^4
})

test_that("label_power10 handles scale parameter correctly", {
  # Test with scale = 1000 (to convert to thousands)
  values <- c(1000000, 2000000)

  labeller_scaled <- label_power10(scale = 1000)
  labeller_default <- label_power10(scale = 1)

  result_scaled <- labeller_scaled(values)
  result_default <- labeller_default(values)

  expect_true(is.expression(result_scaled))
  expect_true(is.expression(result_default))

  # Results should be different due to scaling
  text_scaled <- sapply(result_scaled, deparse)
  text_default <- sapply(result_default, deparse)

  # Scaled version should show 1000x larger exponent
  expect_identical(text_default[1], "1 %*% 10^6") # 1000000 = 10^6
  expect_identical(text_scaled[1], "1 %*% 10^9") # 1000000 * 1000 = 10^9
})

test_that("label_power10 handles vector input correctly", {
  labeller <- label_power10()

  # Test with multiple values
  values <- c(100, 1000, 10000, 100000)
  result <- labeller(values)

  expect_true(is.expression(result))
  expect_identical(length(result), length(values))

  # Test specific results
  text_results <- sapply(result, deparse)
  expect_identical(text_results[1], "1 %*% 10^2") # 100 = 10^2
  expect_identical(text_results[2], "1 %*% 10^3") # 1000 = 10^3
  expect_identical(text_results[3], "1 %*% 10^4") # 10000 = 10^4
  expect_identical(text_results[4], "1 %*% 10^5") # 100000 = 10^5
})

test_that("label_power10 magnitude_only works with vector input", {
  labeller <- label_power10(magnitude_only = TRUE)

  # Test with multiple values in different magnitudes
  values <- c(150, 1500, 15000, 150000)
  result <- labeller(values)

  expect_true(is.expression(result))
  expect_identical(length(result), length(values))

  # Convert to text for comparison
  text_results <- sapply(result, deparse)

  # Should show different magnitudes as pure powers
  expect_identical(text_results[1], "10^2") # 150 -> 10^2
  expect_identical(text_results[2], "10^3") # 1500 -> 10^3
  expect_identical(text_results[3], "10^4") # 15000 -> 10^4
  expect_identical(text_results[4], "10^5") # 150000 -> 10^5
})

test_that("label_power10 handles edge case of zero exponent", {
  labeller_default <- label_power10()
  labeller_magnitude <- label_power10(magnitude_only = TRUE)

  # Values that result in exponent = 0 (between 1 and 10)
  values <- c(5.5, 7.2)
  result_default <- labeller_default(values)
  result_magnitude <- labeller_magnitude(values)

  expect_true(is.expression(result_default))
  expect_true(is.expression(result_magnitude))

  # Both should show the mantissa since exponent is 0
  text_default <- sapply(result_default, deparse)
  text_magnitude <- sapply(result_magnitude, deparse)

  # For exponent 0, both should be identical (just the mantissa)
  expect_identical(text_default[1], "5.5")
  expect_identical(text_magnitude[1], "1")
  expect_identical(text_default[2], "7.2")
  expect_identical(text_magnitude[2], "1")
})

test_that("label_power10 produces expressions that can be plotted", {
  labeller <- label_power10()

  # Test that the expressions work in a plotting context
  values <- c(200, 1000, 10000)
  result <- labeller(values)

  expect_true(is.expression(result))
  expect_identical(length(result), 3L)

  # Each should be a valid expression that can be evaluated
  for (i in seq_along(result)) {
    expect_no_error(eval(result[[i]]))
  }
})

test_that("label_power10 magnitude_only handles edge cases correctly", {
  labeller <- label_power10(magnitude_only = TRUE)

  # Test with values that have mantissa ≈ 1 and small numbers
  values <- c(1000, 10000, 0.001, 0.01)
  result <- labeller(values)

  text_results <- sapply(result, deparse)

  expect_identical(text_results[1], "10^3") # 1000 = 1.0 x 10^3
  expect_identical(text_results[2], "10^4") # 10000 = 1.0 x 10^4
  expect_identical(text_results[3], "10^-3") # 0.001 = 1.0 x 10^-3
  expect_identical(text_results[4], "10^-2") # 0.01 = 1.0 x 10^-2
})
