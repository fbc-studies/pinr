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

test_that("date of birth century can be imputed", {
  expect_equal(pin_dob("131052308T", try_fix = TRUE), as.Date("1952-10-13"))
})
