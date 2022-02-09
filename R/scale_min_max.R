#' scale_min_max
#'
#' Convert vector to unit scale based on minimum and maximum values.
#'
#' The function is implemented as \eqn{(x - min(x)) / (max(x) - min(x))}
#'
#' @param x vector of numeric values.
#'
#' @return vector of values scaled between 0 and 1.
#' @export
#'
#' @examples
#' scale_min_max(1:10)
scale_min_max <- function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
