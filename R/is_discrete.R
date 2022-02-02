is_discrete <- function(x, cutoff = 100) {
  # if (is.data.frame(x)) x <- x[[1]]
  unique_value_count <- length(unique(stats::na.omit(x)))
  ifelse(is.numeric(x) & unique_value_count < cutoff, TRUE, FALSE)
}
