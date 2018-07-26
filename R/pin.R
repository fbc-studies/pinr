#' Create a PIN
#'
#' @param dob Date vector of dates of birth
#' @param ordinal integer giving order of birth on `dob`. Even for female, odd
#'   for male. Takes values between 1 and 999.
#' @export
pin <- function(dob, ordinal) {
  year <- lubridate::year(dob)
  century <- year %/% 100L

  dmy <- dmy_string(dob)
  sep <- century_sep(century)
  end <- new_end(dob, ordinal)

  pin <- paste0(dmy, sep, end)
  pin_validate(pin)
}

dmy_string <- function(date) {
  format(date, format = "%d%m%y")
}

century_sep <- function(century) {
  inverse_map(century, .centuries)
}

new_end <- function(dob, ordinal) {
  dmy <- dmy_string(dob)
  ord <- pad0(ordinal, 3L)

  n <- paste0(dmy, ord)
  n <- as.integer(n)
  check <- check_char(n)

  paste0(ord, check)
}
