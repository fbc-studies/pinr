#' Guess if a vector contains PINs
#'
#' @param x Vector to test for pin-iness.
#' @return Logcial. Is the vector likely to contain pins?
#' @export
is_probably_pin <- function(x) {

  # Tavoitteena "fail fast" eli mahdollisimman nopeasti (ja mahdollisimman
  # vahin tarkistuksin) paatetaan etta kyseessa ei ole hetu. Nain funktiota
  # on nopeampi ajaa, mika on olennaista kun aineistoja on kaytava lapi
  # tuhansia, ja aineistoissa on parhaimmillaan useita satoja sarakkeita.

  # Jos ei ole merkkijono ei voi olla hetu
  if (!is.character(x)) {
    return(FALSE)
  }

  x <- x[!is.na(x) & x != ""]  # poistetaan puuttuvat
  x <- stringr::str_trim(x)    # ja ylimaarainen whitespace

  if (length(x) == 0)
    return(FALSE)

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

  # Hetun "tietosisaltoon" perustuvia tarkistuksia -----

  # Jos syntymapaivaa ei saa paateltya niin ei varmaankaan ole hetu
  invalid_dob <- is.na(lubridate::dmy(pin_get$dob(x)))
  if (mean(invalid_dob) > 0.1) {
    return(FALSE)
  }

  # Lapi paasee viela:
  #  - merkkijonot,
  #  - joissa kaikki < 13 merkkisia,
  #  - ja yli 90 % joko 10 tai 11 merkkisia,
  #  - ja alle 10 % ei saa validia paivamaaraa 6 ekasta merkista

  # Konservatiivisesti sanotaan kylla jos ei muuta aiemmin paateta
  TRUE
}
