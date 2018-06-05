context("PIN extractors")

test_that("sex is extracted correctly", {
  expect_equivalent(pin_sex("311280-888Y", lang = "en"), factor("Female"))
  expect_equivalent(pin_sex("311280-888Y", lang = "fi"), factor("Nainen"))
  expect_equal(pin_sex("311280-888Y", factor = FALSE), 2)

  expect_equivalent(pin_sex("311280-8890", lang = "en"), factor("Male"))
  expect_equivalent(pin_sex("311280-8890", lang = "fi"), factor("Mies"))
  expect_equal(pin_sex("311280-8890", factor = FALSE), 1)
})

test_that("date of birth is extracted correctly", {
  expect_equal(pin_dob("131052+308T"), as.Date("1852-10-13"))
  expect_equal(pin_dob("131052-308T"), as.Date("1952-10-13"))
  expect_equal(pin_dob("131052A308T"), as.Date("2052-10-13"))
})


# Extraction to data frame ------------------------------------------------

df <- data.frame(a = 1:2, pin = c("311280-888Y", "131052-308T"), b = 1:2)

test_that("pin data can be extracted to new columns", {
  out <- pin_extract(df, pin)

  expect_equal(out$dob, as.Date(c("1980-12-31", "1952-10-13")))
  expect_equivalent(out$sex, factor(c("Female", "Female")))
})

test_that("extracted columns come after original", {
  out <- pin_extract(df, pin)
  expect_equal(match(c("dob", "sex"), names(out)), c(3, 4))
})

test_that("can set the name of extracted columns", {
  out <- pin_extract(df, pin, into = c("dateOfBirth", "gender"))

  expect_equal(out$dateOfBirth, as.Date(c("1980-12-31", "1952-10-13")))
  expect_equivalent(out$gender, factor(c("Female", "Female")))
})

test_that("can remove original column", {
  expect_null(pin_extract(df, pin, remove = TRUE)$pin)
})

test_that("new columns are placed correctly when removing old column", {
  out <- pin_extract(df, pin, remove = TRUE)
  expect_equal(match(c("dob", "sex"), names(out)), c(2, 3))
})

