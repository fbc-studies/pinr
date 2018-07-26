context("test-pin.R")

test_that("pin() creates valid PINs", {
  expect_true(is_valid_pin(pin(lubridate::today(), 1)))

  expect_true(is_valid_pin(pin(as.Date("1852-10-13"), 308)))
  expect_true(is_valid_pin(pin(as.Date("1952-10-13"), 308)))
  expect_true(is_valid_pin(pin(as.Date("2052-10-13"), 308)))

  expect_error(pin(as.Date("2152-10-13"), 308), "Invalid")
  expect_error(pin(as.Date("1952-10-13"), 3080), "Invalid")
})

test_that("pin() can recreate a PIN", {
  pin <- c("311280-888Y", "131052-308T")
  expect_equal(pin(pin_dob(pin), pin_ordinal(pin)), pin)
})
