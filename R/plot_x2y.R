#' Plot x2y
#'
#'
#' @param df data frame
#' @param eda_var character
#' @param target_var character name of column for conditional analysis
#' @param reference_var character vector of one or more columns to be used for comparison with \code{eda_var}
#' @param width numeric width of plot in pixels
#' @param height numeric height of plot in pixels
#'
#' @return NULL. Function called for side effect of rendering exploratory data
#'   anlysis visualization
#' @export
#'
#' @examples
#' # plot_conditional_distribution()
plot_x2y <- function(df,
                     eda_var = NULL,
                     target_var = NULL,
                     reference_var = NULL,
                     width = 300,
                     height = 450) {
  # binding variable just to keep R CMD Check from seeing NSE as global variables

  png_file <- ""

  plot_output <-
    df %>%
    dplyr::select(dplyr::any_of(c(eda_var, target_var, reference_var))) %>%
    lares::x2y(
      target = target_var,
      symmetri = FALSE,
      plot = TRUE
    ) +
    ggplot2::labs(
      title = "Cross-feature predictive power",
      subtitle = NULL)


  png_file <- tempfile(fileext = ".png")
  grDevices::png(
    png_file,
    width = width,
    height = height
  )
  print(plot_output)
  grDevices::dev.off()

  return(png_file)
}
