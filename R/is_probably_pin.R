#' Guess if a vector contains PINs
#'
#' @param x Vector to test for pin-iness.
#' @param ... Further arguments passed down to methods. Currently unused.
#' @return Logcial. Is the vector likely to contain pins?
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

  # Tavoitteena "fail fast" eli mahdollisimman nopeasti (ja mahdollisimman
  # vahin tarkistuksin) paatetaan etta kyseessa ei ole hetu. Nain funktiota
  # on nopeampi ajaa, mika on olennaista kun aineistoja on kaytava lapi
  # tuhansia, ja aineistoissa on parhaimmillaan useita satoja sarakkeita.

  x <- x[!is.na(x) & x != ""]  # drop missing an empty strings
  x <- stringr::str_trim(x)    # as well as extra whitespace

  if (length(x) == 0) {
    return(FALSE)
  }

  # Merkkijonojen pituuteen perustuvia tarkistuksia -----

  n_char <- nchar(x)

  if (max(n_char) > 12) {
    return(FALSE)
  }

  hetu_mitat <- c(
    0L,   # tyhja rivi
    10L,  # epataydellinen hetu
    11L   # taydellinen hetu
  )

  p_in_mitat <- mean(n_char %in% hetu_mitat)

  # Jos alle 90 % on hetun mittaisia niin ei varmaankaan hetu
  if (p_in_mitat < 0.9) {
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

  # Lapi paasee viela:
  #  - merkkijonot,
  #  - joissa kaikki < 13 merkkisia,
  #  - ja yli 90 % joko 10 tai 11 merkkisia,
  #  - ja alle 10 % ei saa validia paivamaaraa 6 ekasta merkista

  # Konservatiivisesti sanotaan kylla jos ei muuta aiemmin paateta
  TRUE
}
