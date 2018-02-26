#' Pseudonymize columns in data containing PINs
#'
#' @param data A data frame containing PINs to be pseudonymized.
#' @param key Named vector or data frame, used as a lookup table for pids. If
#'   data frame, the first column is assumed to contain PINs and the second
#'   column the corresponding pids.
#' @param drop_pin Logical. Should identified PIN columns be dropped?
#' @param pid_suffix Character scalar. Added to the end of the pin column names
#'   for the corresponding pid columns if \code{drop_pin = FALSE}.
#' @return A data frame where PINs have probably been linked to pids. If
#'   \code{drop_pin = TRUE} values in columns guessed to have PINs have been
#'   replaced with matching pids from `key`.
#' @seealso \code{\link{is_probably_pin}} used to guess if columns contain PINs
#' @export
pseudonymize <- function(data, key, ..., drop_pin = TRUE, pid_suffix = "_pid", guess = FALSE) {

  if (is.data.frame(key)) {
    key <- tibble::deframe(key)
  }

  selected <- tidyselect::vars_select(names(data), !!!rlang::quos(...))
  any_manual <- length(selected) > 0

  if (!any_selected && !guess) {
    msg <- paste0(
      "No columns selected to pseudonymize. ",
      "Did you forget to set `guess = TRUE`?"
    )
    warning(msg, call. = FALSE)
    return(data)
  }

  is_pin <- names(data) %in% selected
  pin_cols <- if (any_manual) dplyr::select(data, !!!selected) else character(0)

  if (guess) {
    probably_pin <- purrr::map_lgl(data, is_probably_pin)
    probably_pin_cols <- data[probably_pin]

    is_pin <- is_pin | probably_pin
    pin_cols <- probably_pin_cols
  }

  pid_cols <- purrr::map(pin_cols, ~ key[match(.x, names(key))])
  names(pid_cols) <- paste0(names(pin_cols), pid_suffix)

  if (drop_pin) {
    data[is_pin] <- pid_cols
    data <- if (any_manual) dplyr::rename(data, !!!selected) else data
    return(data)
  }

  dplyr::bind_cols(data, pid_cols)
}

#' @rdname pseudonymize
pseudonymise <- pseudonymize
