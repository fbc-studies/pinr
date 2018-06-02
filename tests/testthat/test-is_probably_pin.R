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


# Not matching ------------------------------------------------------------

test_that("numeric columns are not identified", {
  expect_false(is_probably_pin(131052308))
})

test_that("fully missing or empty columns are not identified", {
  expect_false(is_probably_pin(NA_character_))
  expect_false(is_probably_pin(c("", "", "")))
  expect_false(is_probably_pin(c(NA, NA, NA)))
})

test_that("columns with long strings are not identified", {
  expect_false(is_probably_pin("This is a rather long string."))
  expect_false(is_probably_pin("120345-12345679"))
})

test_that("columns with short strings are not identified", {
  expect_false(is_probably_pin("Brief."))
  expect_false(is_probably_pin("120345-12"))
})

test_that("columns that can't parse into a valid date are not identified", {
  expect_false(is_probably_pin("123456-7890"))
  expect_false(is_probably_pin("098765+4321"))
})
