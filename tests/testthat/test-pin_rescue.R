context("test-pin_rescue.R")

test_that("pin_rescue produces valid PINs when possible", {
  expect_true(is_valid_pin(pin_rescue("131052308T")))
})

test_that("separator selection works", {
  # default
  expect_equal(pin_rescue("131052308T"), "131052-308T")

  # valid
  expect_equal(pin_rescue("131052308T", "+"), "131052+308T")
  expect_equal(pin_rescue("131052308T", "-"), "131052-308T")
  expect_equal(pin_rescue("131052308T", "A"), "131052A308T")

  # invalid
  expect_error(pin_rescue("131052308T", "B"), "should be one of")
})

test_that("valid separators are preserved", {
  expect_equal(pin_rescue("131052+308T"), "131052+308T")
  expect_equal(pin_rescue("131052-308T"), "131052-308T")
  expect_equal(pin_rescue("131052A308T"), "131052A308T")
})

test_that("validate works in pin_rescue", {
  expect_error(pin_rescue("invalid", validate = TRUE), "invalid")
  expect_silent(pin_rescue("invalid", validate = FALSE))
})
