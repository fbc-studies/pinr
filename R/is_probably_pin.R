#' Guess if a vector contains PINs
#'
#' @param x Vector to test for pin-iness.
#' @param ... Further arguments passed down to methods. Currently unused.
#' @return Logical. Is the vector likely to contain pins?
#' @export
is_probably_pin <- function(x, ...) {
  UseMethod("is_probably_pin")
}

#' @export
is_probably_pin.default <- function(x, ...) {
  # A collection of PINs is almost certainly a character vector or factor:
  # this is due to the presence of check symbols and century separators
  FALSE
}

#' @export
is_probably_pin.factor <- function(x, ...) {
  is_probably_pin(as.character(x), ...)
}

#' @export
is_probably_pin.character <- function(x, ...) {

  # Try to fail fast: return FALSE as soon as possible. Important when checking
  # hundreds of columns in thousands of data sets.

  x <- x[!is.na(x) & x != ""]  # drop missing an empty strings
  x <- stringr::str_trim(x)    # as well as extra whitespace

  # was everything missing or empty?
  if (length(x) == 0) {
    return(FALSE)
  }

  n_char <- nchar(x)

  # are there any way too long strings? (bit risky)
  if (max(n_char) > 12) {
    return(FALSE)
  }

  pin_lens <- c(
    0L,   # empty row
    10L,  # pin missing separator
    11L   # complete pin
  )

  p_in_lens <- mean(n_char %in% pin_lens)

  # Guess no if less than 90% have a valid length
  if (p_in_lens < 0.9) {
    return(FALSE)
  }

  # Checks based on information contained in PINs -----

  # Significant portion not having valid dates of birth
  if (mean(has_valid_dob(x)) < 0.9) {
    return(FALSE)
  }

  # Validity of separators should NOT be checked: sometimes
  # PINs were stored with length 10 with the separator removed,
  # the implicit assumption being a `-` separator.

  # Guess yes if:
  #  - everything has < 13 characters
  #  - and > 90% have 10 or 11 characters
  #  - and < 10% don't have a valid date in the first 6 characters

  # Guess yes if none of the earlier criteria apply
  TRUE
}

#' @export
is_probably_pin.data.frame <- function(x, ...) {
  any(vapply(x, is_probably_pin, logical(1)))
}
