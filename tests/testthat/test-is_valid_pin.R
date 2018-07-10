context("test-is_valid_pin.R")

test_that("pin validity checks work", {
  pins <- c("311280-888Y", "131052-308T")
  expect_equal(is_valid_pin(pins), c(TRUE, TRUE))
})
