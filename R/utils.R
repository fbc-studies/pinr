map_to_named <- function(x, key) {
  unname(key)[match(as.character(x), names(key))]
}

inverse_map <- function(x, key) {
  names(key)[match(x, key)]
}

insert_cols <- function(data, pid_cols, pos, replace) {
  if (replace) {
    replace_cols(data, pos, pid_cols)
  } else {
    add_cols(data, pid_cols, pos)
  }
}

replace_cols <- function(data, pos, cols) {
  stopifnot(length(pos) == length(cols))

  data[pos] <- cols
  names(data)[pos] <- names(cols)

  data
}

#' Add new columns to a data frame
#'
#' Extends `tibble::add_column` to insert columns to multiple positions.
#'
#' @param data a data frame where new columns are inserted
#' @param cols named list of columns to add to `data`
#' @param pos integer vector of positions to insert columns after
#' @noRd
add_cols <- function(data, cols, pos = ncol(data)) {
  if (length(pos) == 1) {
    # faster to avoid the loop since `add_column()` already
    # handles multiple insertion to single position
    data <- tibble::add_column(data, !!!cols, .after = pos)
    return(data)
  }

  stopifnot(length(cols) == length(pos))

  for (i in rev(seq_along(cols))) {
    data <- tibble::add_column(data, !!!cols[i], .after = pos[i])
  }

  data
}

str_append <- function(x, y, after = nchar(x)) {
  head <- stringr::str_sub(x, 1L, after)
  tail <- stringr::str_sub(x, after + 1L)
  paste0(head, y, tail)
}

pad0 <- function(x, width, side = "left") {
  stringr::str_pad(x, width, pad = "0", side = side)
}

list_message <- function(x, truncate = 5) {
  n <- length(x)

  head <- head(x, truncate)
  msg <- paste("  *", head, collapse = "\n")

  if (n > truncate) {
    more <- comma(n - truncate)
    msg <- paste0(msg, "\n... and ", more, " more.")
  }

  msg
}

comma <- function(x, ...) {
  format(x, big.mark = ",", scientific = FALSE, ...)
}
