context("test-pseudonymize.R")

## TODO: Add smaller tests for component functions to stop coalescing failures.

df <- data.frame(pin = c("311280-888Y", "311280-888Y", "131052-308T"), a = 1:3)

key <- c("311280-888Y" = 1, "131052-308T" = 2)
key_df <- data.frame(pin = names(key), pid = key)

test_that("manually selecting columns works", {
  expect_equal(pseudonymize(df, key, pin)$pin, c(1, 1, 2))
})

test_that("can rename manually selected column", {
  out <- pseudonymize(df, key, pid = pin)

  expect_equal(out$pid, c(1, 1, 2))
  expect_equal(out$pin, NULL)
  expect_equal(match("pid", names(out)), 1)
})

test_that("default rename works with manual selection", {
  out <- pseudonymize(df, key, pin, rename = TRUE)
  expect_equal(names(out)[1], "pin_pid")
})

test_that("default rename works with guessed selection", {
  out <- pseudonymize(df, key, guess = TRUE, rename = TRUE)
  expect_equal(names(out)[1], "pin_pid")
})

test_that("can rename selected columns with a function", {
  out <- pseudonymize(df, key, pin, rename = toupper)
  expect_equal(names(out)[1], "PIN")
})

test_that("rename throws error if not logical or function", {
  expect_error(pseudonymize(df, key, pin, rename = NULL), "`rename` must be")
})

test_that("can pass data frame as key", {
  expect_equal(pseudonymize(df, key_df, pin)$pin, c(1, 1, 2))
})

test_that("can retain pin columns", {
  out1 <- pseudonymize(df, key, pin, replace = FALSE)

  expect_equal(out1$pin, df$pin)
  expect_equal(out1$pin_pid, c(1, 1, 2))
})

test_that("pid columns are insterted after pin columns", {
  df2 <- transform(df, pin2 = pin)
  out <- pseudonymize(df2, key, pid1 = pin, pid2 = pin2, replace = FALSE)

  expect_equal(match(c("pid1", "pid2"), names(out)), c(2, 5))
})

test_that("can manually name new column when retaining pin", {
  out2 <- pseudonymize(df, key, pid = pin, replace = FALSE)

  expect_equal(out2$pin, df$pin)
  expect_equal(out2$pid, c(1, 1, 2))
})

test_that("missing selection warns and returns data unchanged", {
  expect_warning({
    expect_equal(pseudonymize(df, key), df)
  }, "No columns selected to pseudonymize")

  expect_warning({
    expect_equal(pseudonymize(df, key, guess = FALSE), df)
  }, "No columns selected to pseudonymize")
})

test_that("guessing finds valid pin columns", {
  df$pin2 <- df$pin
  out <- pseudonymize(df, key, guess = TRUE)

  expect_equal(out$pin,  c(1, 1, 2))
  expect_equal(out$pin2, c(1, 1, 2))
})

test_that("fully NA pseudonymized columns warn if quiet = TRUE", {
  df <- data.frame(pin = c("311280888Y", "311280888Y", "131052308T"), a = 1:3)

  expect_warning(pseudonymize(df, key, pid = pin), "all `NA` values")
  expect_silent(pseudonymize(df, key, pid = pin, quiet = TRUE))

  expect_warning(pseudonymize(df, key, a), "all `NA` values")
  expect_silent(pseudonymize(df, key, a, quiet = TRUE))
})
