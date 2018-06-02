#' Extract data from PIN
#' @name pin_extractors
NULL

#' @param x Character vector of PINs.
#' @param try_fix Logical. Should missing century be replaced with 19?
#' @describeIn pin_extractors Extract date of birth from PIN
#' @export
pin_dob <- function(x, try_fix = FALSE) {
  dd <- as.integer(stringr::str_sub(x, 1L, 2L))
  mm <- as.integer(stringr::str_sub(x, 3L, 4L))
  yy <- as.integer(stringr::str_sub(x, 5L, 6L))

  century <- pin_century(x)

  if (try_fix) {
    century <- ifelse(is.na(century), 19L, century)
  }

  lubridate::make_date(yy + century * 100L, mm, dd)
}

.centuries <- c("+" = 18L, "-" = 19L, "A" = 20L)

pin_century <- function(x) {
  .centuries[match(pin_get$sep(x), names(.centuries))]
}


#' @param factor Logical. Should the return value be a factor?
#' @param language Character scalar. Language to use for factor labels if
#'   \code{factor = TRUE}. Currently either \code{english} or \code{finnish}.
#' @describeIn pin_extractors Extract sex from PIN
#' @export
pin_sex <- function(x, factor = TRUE, language = c("english", "finnish")) {
  x <- 2L - as.numeric(stringr::str_sub(x, 10L, 10L)) %% 2L
  if (factor) {
    lang <- match.arg(language)
    labs <- switch(lang,
      english = c("Male", "Female"),
      finnish = c("Mies", "Nainen")
    )
    x <- factor(x, levels = 1:2, labels = labs)
  }
  x
}

pin_get <- list(
  dob   = function(x) stringr::str_sub(x, 1L,  6L),
  sep   = function(x) stringr::str_sub(x, 7L,  7L),
  end   = function(x) stringr::str_sub(x, 8L,  10L),
  check = function(x) stringr::str_sub(x, 11L, 11L)
)
