#' Attempt to fix common problems in PINs
#'
#' Currently checks if the separator is valid and inserts `default_sep` if not.
#'
#' @param x character vector of PINs
#' @param default_sep character to use as default separator
#' @param validate should result be checked to ensure all PINs are valid?
#' @export
pin_rescue <- function(x, default_sep = c("-", "A", "+"), validate = FALSE) {
  sep <- match.arg(default_sep)
  x <- ifelse(nchar(x) != 10L, x, fix_sep(x, sep))
  if (validate) pin_validate(x) else x
}

fix_sep <- function(x, default_sep) {
  ifelse(has_valid_sep(x), x, insert_sep(x, default_sep))
}

insert_sep <- function(x, sep) {
  str_append(x, sep, after = 6L)
}
