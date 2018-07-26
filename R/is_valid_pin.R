#' Check if valid PIN
#' @param x vector to check for PIN validity
#' @param ... additional arguments passed down to methods. Currently unused.
#' @return logical vector of `length(x)`
#' @export
is_valid_pin <- function(x, ...) {
  UseMethod("is_valid_pin")
}

#' @export
is_valid_pin.default <- function(x, ...) {
  rep_len(FALSE, length(x))
}

#' @export
is_valid_pin.factor <- function(x, ...) {
  is_valid_pin(as.character(x), ...)
}

#' @export
is_valid_pin.character <- function(x, ...) {
  has_valid_dob(x) & has_valid_sep(x) & has_valid_checksum(x)
}

has_valid_dob <- function(x) {
  !is.na(lubridate::dmy(pin_get$dob(x), quiet = TRUE))
}

has_valid_sep <- function(x) {
  !is.na(pin_century(x))
}

has_valid_checksum <- function(x) {
  given <- pin_get$check(x)
  calculated <- pin_checksum(x)
  !is.na(calculated) & calculated == given
}

#' Calculate the checksum from a PIN
#' @param x vector of PINs
#' @export
pin_checksum <- function(x) {
  n <- paste0(pin_get$dob(x), pin_get$end(x))
  n <- suppressWarnings(as.integer(n))
  check_char(n)
}

check_char <- function(n) {
  .check_chars[n %% 31L + 1L]
}

.check_chars <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                  "A", "B", "C", "D", "E", "F", "H", "J", "K", "L",
                  "M", "N", "P", "R", "S", "T", "U", "V", "W", "X", "Y")

pin_validate <- function(x) {

  # Placeholder function that ensures PINs are valid: a more thorough
  # implementation would throw more informative errors.

  invalid <- !is.na(x) & !is_valid_pin(x) & x != ""

  if (any(invalid)) {
    n <- sum(invalid)
    msg <- paste0("  * ", head(x[invalid], 5), collapse = "\n")
    if (n > 5) {
      msg <- paste0(msg, "\n... and ", n - 5, " more.")
    }
    stop("Invalid PINs found:\n", msg, call. = FALSE)
  }

  x
}
