map_to_named <- function(x, key) {
  unname(key)[match(as.character(x), names(key))]
}

inverse_map <- function(x, key) {
  names(key)[match(x, key)]
}

#' Add new columns to a data frame
#'
#' Extends `tibble::add_column` to insert columns to multiple positions.
#'
#' @param data a data frame where new columns are inserted
#' @param cols named list of columns to add to `data`
#' @param pos integer vector of positions to insert columns after
#' @noRd
add_cols <- function(data, cols, pos) {
  if (length(pos) == 1) {
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