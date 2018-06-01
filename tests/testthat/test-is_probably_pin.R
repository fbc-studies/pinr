context("test-is_probably_pin.R")

test_that("valid pins are identified", {
  # test pin from: http://vrk.fi/en/personal-identity-code1
  expect_true(is_probably_pin("131052-308T"))
})
