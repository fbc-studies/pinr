context("test-is_valid_pin.R")

test_that("pin validity checks work", {
  expect_true(is_valid_pin("131052+308T"))
  expect_true(is_valid_pin("131052-308T"))
  expect_true(is_valid_pin("131052A308T"))

  expect_false(is_valid_pin("131052-308U"))
  expect_false(is_valid_pin("131052-309T"))
  expect_false(is_valid_pin("131052_308T"))
  expect_false(is_valid_pin("132052-308T"))
  expect_false(is_valid_pin("431052-308T"))

  expect_false(is_valid_pin("131052308T"))
  expect_false(is_valid_pin("131052-000"))

  expect_false(is_valid_pin(3112808890))
  expect_false(is_valid_pin(NA))
  expect_false(is_valid_pin(NA_character_))
  expect_false(is_valid_pin(""))

  pins <- c("311280-888Y", "131052-308T")
  expect_equal(is_valid_pin(pins), rep(TRUE, 2))
  expect_equal(is_valid_pin(factor(pins)), rep(TRUE, 2))

  expect_equal(is_valid_pin(c("131052-308T", "")), c(TRUE, FALSE))
})
