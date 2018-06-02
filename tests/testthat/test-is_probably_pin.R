context("test-is_probably_pin.R")

test_that("valid pins are identified", {
  # test pins from public sources:
  #   http://vrk.fi/en/personal-identity-code1
  #   https://en.wikipedia.org/wiki/National_identification_number#Finland
  x <- c("311280-888Y", "311280-888Y", "131052-308T")
  expect_true(is_probably_pin("131052-308T"))
  expect_true(is_probably_pin(x))
})
