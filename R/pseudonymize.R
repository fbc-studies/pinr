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
#' @param replace Logical. Should PIN columns be replaced with the pseudonymized
#'   versions?
#' @param pid_suffix Character scalar. Added to the end of the pin column names
#'   for the corresponding pid columns if \code{replace = FALSE}.
#' @return A data frame where PINs have probably been linked to pids. If
#'   \code{replace = TRUE} values in columns guessed to have PINs have been
#'   replaced with matching pids from `key`.
#' @seealso \code{\link{is_probably_pin}} used to guess if columns contain PINs
#' @export
pseudonymize <- function(data, key, ..., guess = FALSE,
                         replace = TRUE, pid_suffix = "_pid") {
  if (is.data.frame(key)) {
    key <- tibble::deframe(key)
  }

  nm <- names(data)
  dots <- rlang::quos(...)

  manual <- tidyselect::vars_select(nm, !!!dots)
  any_manual <- length(manual) > 0

  if (!any_manual && !guess) {
    msg <- paste0(
      "No columns selected to pseudonymize. ",
      "Did you forget to set `guess = TRUE`?"
    )
    warning(msg, call. = FALSE)
    return(data)
  }

  is_pin <- nm %in% manual

  if (guess) {
    maybe <- purrr::map_lgl(data, is_probably_pin)
    is_pin <- is_pin | maybe
  }

  pid_cols <- purrr::map(data[is_pin], map_to_named, key)
  new <- tidyselect::vars_rename(nm, !!!manual)

  if (replace) {
    data[is_pin] <- pid_cols
    names(data) <- names(new)
  } else {
    # original names should be preserved: set names of pid_cols
    pin_nm <- new[is_pin]
    pid_nm <- names(new)[is_pin]

    suffixed <- paste0(pid_nm, pid_suffix)
    pid_nm <- ifelse(pid_nm != pin_nm, pid_nm, suffixed)

    names(pid_cols) <- pid_nm
    data <- cbind(data, pid_cols)
  }

  data
}

#' @rdname pseudonymize
pseudonymise <- pseudonymize

map_to_named <- function(x, key) {
  unname(key)[match(as.character(x), names(key))]
}
