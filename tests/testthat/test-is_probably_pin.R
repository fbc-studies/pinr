context("test-is_probably_pin.R")

test_that("valid pins are identified", {
  # test pins from public sources:
  #   http://vrk.fi/en/personal-identity-code1
  #   https://en.wikipedia.org/wiki/National_identification_number#Finland
  expect_true(is_probably_pin("131052-308T"))
  expect_true(is_probably_pin(c("311280-888Y", "131052-308T")))
})

test_that("factors can be identified", {
  expect_true(is_probably_pin(factor("131052-308T")))
})

test_that("numeric data are not identified", {
  expect_false(is_probably_pin(131052308))
})
