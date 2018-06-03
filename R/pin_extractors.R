#' Extract data from PIN
#' @name pin_extractors
NULL

#' @param data A data frame with a PIN column.
#' @param pin The name of the column cotaining PINs. Can be a bare
#'   name or a string. Uses `tidyselect` semantics to pick the column
#'   from the data.
#' @param into A character vector of length 2. Gives the names of the
#'   new columns that date of birth and sex are extracted into.
#' @param remove Logical. Should the original PIN column be removed?
#' @param ... Additional arguments passed to `pin_sex`.
#' @describeIn pin_extractors Extract date of birth and sex into new
#'   columns in a supplied data frame.
#' @export
pin_extract <- function(data, pin, into = c("dob", "sex"),
                        remove = FALSE, ...) {
  nm <- names(data)

  pin <- tidyselect::vars_pull(nm, !!rlang::enquo(pin))
  pos <- match(pin, nm)

  pin <- data[[pin]]
  if (remove) {
    data[[pos]] <- NULL
  }

  dob <- pin_dob(pin)
  sex <- pin_sex(pin, ...)

  new <- list(dob, sex)
  names(new) <- into

  tibble::add_column(data, !!!new, .after = pos)
}

#' @param x Character vector of PINs.
#' @describeIn pin_extractors Extract date of birth from PIN
#' @export
pin_dob <- function(x) {
  dd <- as.integer(stringr::str_sub(x, 1L, 2L))
  mm <- as.integer(stringr::str_sub(x, 3L, 4L))
  yy <- as.integer(stringr::str_sub(x, 5L, 6L))

  century <- pin_century(x)
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
  x <- 2L - as.integer(pin_get$end(x)) %% 2L
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
