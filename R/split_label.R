split_label <- function (x) {
  x <- stringr::str_replace_all(x, "_", " ")
  lapply(strwrap(as.character(x), width = 15, simplify = FALSE),
         paste, collapse = "\n")
}
