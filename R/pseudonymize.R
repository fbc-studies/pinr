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
#' @param rename Logical or function. If `FALSE`, pseudonymized columns will
#'   not be automatically renamed; if `TRUE`, they will be suffixed with
#'   `"_pid"`; if a function, will be called on PIN column names to generate
#'   new names for the pseudonymized columns. Manually specified new names will
#'   always be used regardless.
#' @return A data frame where PINs have probably been linked to pids. If
#'   \code{replace = TRUE} values in columns guessed to have PINs have been
#'   replaced with matching pids from `key`.
#' @seealso \code{\link{is_probably_pin}} used to guess if columns contain PINs
#' @export
pseudonymize <- function(data, key, ..., guess = FALSE,
                         replace = TRUE, rename = !replace) {
  if (is.data.frame(key)) {
    key <- tibble::deframe(key)
  }

  if (is.logical(rename)) {
    if (rename) {
      rename <- function(x) paste0(x, "_pid")
    } else {
      rename <- base::identity
    }
  } else if (!is.function(rename)) {
    stop("`rename` must be logical or a function, not ",
         typeof(rename), call. = FALSE)
  }

  nm <- names(data)
  manual <- tidyselect::vars_select(nm, ...)

  any_manual <- length(manual) > 0
  if (!any_manual && !guess) {
    warning("No columns selected to pseudonymize. ",
            "Did you forget to set `guess = TRUE`?", call. = FALSE)
    return(data)
  }

  is_pin <- nm %in% manual

  if (guess) {
    maybe <- purrr::map_lgl(data, is_probably_pin)
    is_pin <- is_pin | maybe
  }

  pid_cols <- purrr::map(data[is_pin], map_to_named, key)
  new_nm <- names(tidyselect::vars_rename(nm, !!!manual))

  to_rename <- new_nm == nm & is_pin
  new_nm[to_rename] <- rename(new_nm[to_rename])

  if (replace) {
    data[is_pin] <- pid_cols
    names(data) <- new_nm
  } else {
    pin_pos <- which(is_pin)
    pid_nm <- new_nm[is_pin]
    names(pid_cols) <- pid_nm

    data <- add_cols(data, pid_cols, pin_pos)
  }

  data
}

#' @rdname pseudonymize
pseudonymise <- pseudonymize

map_to_named <- function(x, key) {
  unname(key)[match(as.character(x), names(key))]
}

#' Add new columns to a data frame
#'
#' Extends `tibble::add_column` to insert columns to multiple positions.
#'
#' @param data a data frame where new columns are inserted
#' @param cols named list of columns to add to `data`
#' @param pos positions to insert columns after
add_cols <- function(data, cols, pos) {
  stopifnot(length(cols) == length(pos))

  for (i in rev(seq_along(cols))) {
    data <- tibble::add_column(data, !!!cols[i], .after = pos[i])
  }

  data
}
