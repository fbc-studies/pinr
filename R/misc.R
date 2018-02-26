str_insert <- function(x, what, after = nchar(x)) {
  x <- str_split_at(x, after)
  x <- purrr::map(x, append, what, after = 1L)
  purrr::map_chr(x, paste, collapse = "")
}

str_split_at <- function(x, pos) {
  head <- substring(x, 1L, pos)
  tail <- substring(x, pos + 1L)
  purrr::map2(head, tail, c)
}
