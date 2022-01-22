is_discrete <- function(x, cutoff = 100) {
  stopifnot("x is not numeric" = is.numeric(x))
  unique_value_count <- length(unique(stats::na.omit(x)))
  ifelse(unique_value_count < cutoff, TRUE, FALSE)
}
