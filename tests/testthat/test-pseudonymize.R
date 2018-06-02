context("test-pseudonymize.R")

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

test_that("can pass data frame as key", {
  expect_equal(pseudonymize(df, key_df, pin)$pin, c(1, 1, 2))
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
