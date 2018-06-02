#' Pseudonymize columns in data containing PINs
#'
#' @param data A data frame containing PINs to be pseudonymized.
#' @param key Named vector or data frame, used as a lookup table for pids. If
#'   data frame, the first column is assumed to contain PINs and the second
#'   column the corresponding pids.
#' @param ... Manually selected columns to be pseudonymized. These are
#'   automatically quoted and evaluated in the context of the data. Uses
#'   `tidyselect` semantics for selection.
#' @param guess Logical. Attempt to automatically identify and pseudonymize
#'   columns that contain PINs?
#' @param remove Logical. Should identified PIN columns be dropped?
#' @param pid_suffix Character scalar. Added to the end of the pin column names
#'   for the corresponding pid columns if \code{remove = FALSE}.
#' @return A data frame where PINs have probably been linked to pids. If
#'   \code{remove = TRUE} values in columns guessed to have PINs have been
#'   replaced with matching pids from `key`.
#' @seealso \code{\link{is_probably_pin}} used to guess if columns contain PINs
#' @export
pseudonymize <- function(data, key, ..., guess = FALSE,
                         remove = TRUE, pid_suffix = "_pid") {

  if (is.data.frame(key)) {
    key <- tibble::deframe(key)
  }

  selected <- tidyselect::vars_select(names(data), !!!rlang::quos(...))
  any_manual <- length(selected) > 0

  if (!any_manual && !guess) {
    msg <- paste0(
      "No columns selected to pseudonymize. ",
      "Did you forget to set `guess = TRUE`?"
    )
    warning(msg, call. = FALSE)
    return(data)
  }

  is_pin <- names(data) %in% selected

  if (guess) {
    probably_pin <- purrr::map_lgl(data, is_probably_pin)
    is_pin <- is_pin | probably_pin
  }

  pin_cols <- data[is_pin]
  pid_cols <- purrr::map(pin_cols, map_to_named, key)

  names(pid_cols) <- paste0(names(pin_cols), pid_suffix)

  if (remove) {
    data[is_pin] <- pid_cols
    data <- if (any_manual) dplyr::rename(data, !!!selected) else data
    return(data)
  }

  dplyr::bind_cols(data, pid_cols)
}

#' @rdname pseudonymize
pseudonymise <- pseudonymize

map_to_named <- function(x, key) {
  unname(key)[match(as.character(x), names(key))]
}
