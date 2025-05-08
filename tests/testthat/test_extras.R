test_that("Test package onLoad for plotly", {
  expect_no_error(suppressPackageStartupMessages(suppressWarnings(library(plotly))))
})