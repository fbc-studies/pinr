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
#' @param rename Logical or function. If `FALSE`, pseudonymized columns will not
#'   be automatically renamed; if `TRUE`, they will be suffixed with `"_pid"`;
#'   if a function, will be called on PIN column names to generate new names for
#'   the pseudonymized columns. Manually specified new names will always be used
#'   regardless.
#' @param quiet Suppress additional messages? Currently controls showing a
#'   warning if pseudonymization results in all `NA` values in some column.
#' @return A data frame where PINs have probably been linked to pids. If
#'   \code{replace = TRUE} values in columns guessed to have PINs have been
#'   replaced with matching pids from `key`.
#' @seealso \code{\link{is_probably_pin}} used to guess if columns contain PINs
#' @export
pseudonymize <- function(data, key, ..., guess = FALSE, replace = TRUE,
                         rename = !replace, quiet = FALSE) {
  manual <- manual_select(data, ...)
  if (nothing_selected(manual, guess)) {
    return(data)
  }

  key <- validate_pskey(key) # if key is invalid, stop early
  pin_pos <- which_pins(data, manual, guess)

  pid_cols <- lapply(data[pin_pos], pseudonymize_col, key)
  if (!quiet) warn_if_all_na(pid_cols)

  pid_cols <- rename_pid(pid_cols, manual, rename)
  insert_cols(data, pid_cols, pin_pos, replace)
}

#' @rdname pseudonymize
pseudonymise <- pseudonymize

pseudonymize_col <- function(x, key) {
  # Placeholder: might turn `pseudonymize()` generic instead.
  # Consider: documentation, code completion.
  map_to_named(x, key)
}

# Workhorses --------------------------------------------------------------

#' @noRd
#' @return named character vector of selection with new names as names
manual_select <- function(data, ...) {
  tidyselect::vars_select(names(data), ...)
}

nothing_selected <- function(selection, guess) {
  if (nothing <- length(selection) == 0 && !guess) {
    warning("No columns selected to pseudonymize. ",
            "Did you forget to set `guess = TRUE`?",
            call. = FALSE, immediate. = TRUE)
  }
  nothing
}

validate_pskey <- function(key) {
  stopifnot(!is.null(names(key)) || is.data.frame(key))

  if (is.data.frame(key))
    tibble::deframe(key) else key
}

which_pins <- function(data, manual = character(0), guess = TRUE) {
  probably_pin <- if (guess)
    vapply(data, is_probably_pin, logical(1)) else FALSE

  which(probably_pin | names(data) %in% manual)
}

warn_if_all_na <- function(data) {
  all_na <- vapply(data, function(x) all(is.na(x)), logical(1))

  if (any(all_na)) {
    all_na <- paste0("`", names(data)[all_na], "`")
    warning("Pseudonymizing column(s) resulted in all `NA` values:\n",
            list_message(all_na), "\nDo the columns have invalid PINs",
            " or is they key incomplete?", call. = FALSE, immediate. = TRUE)
  }

  invisible(data)
}

rename_pid <- function(data, manual, rename) {
  # unchanged manuals should still be renamed
  manual <- manual[manual != names(manual)]
  rename <- get_rename_fun(rename)

  nm <- names(data)
  skip <- nm %in% manual

  # don't rename manually changed names
  names(data)[!skip] <- rename(nm[!skip])
  names(data)[skip] <- names(manual)[skip]

  data
}

get_rename_fun <- function(rename) {
  if (is.logical(rename)) {
    stopifnot(!is.na(rename))
    rename <- if (rename)
      function(x) paste0(x, "_pid") else identity
  } else if (!is.function(rename)) {
    stop("`rename` must be logical or a function, not ",
         typeof(rename), call. = FALSE)
  }

  rename
}
