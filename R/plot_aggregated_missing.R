#' Plot_aggregated_missing
#'
#'
#' @param df data frame
#' @param eda_var character
#' @param target_var character name of column for conditional analysis
#' @param reference_var character vector of one or more columns to be used for comparison with \code{eda_var}
#' @param plot_colors character vector of plot colors (non-missing values, missing values)
#' @param width numeric width of plot in pixels
#' @param height numeric height of plot in pixels
#'
#' @return NULL. Function called for side effect of rendering exploratory data
#'   anlysis visualization
#' @export
#'
#' @examples
#' # plot_conditional_distribution()
plot_aggregated_missing <- function(df,
                     eda_var = NULL,
                     target_var = NULL,
                     reference_var = NULL,
                     plot_colors = c("#949398", "#F4DF4E"), # Ultimate Gray, Illuminating
                     width = 300,
                     height = 450) {
  # binding variable just to keep R CMD Check from seeing NSE as global variables

  png_file <- ""

  png_file <- tempfile(fileext = ".png")
  grDevices::png(
    png_file,
    width = width,
    height = height
  )
  df %>%
    dplyr::select(dplyr::any_of(c(eda_var, target_var, reference_var))) %>%
  VIM::aggr(
    col = plot_colors,
    combined = TRUE,
    cex.axis = .9,
    oma = c(10, 5, 5, 3)
  )
  grDevices::dev.off()

  return(png_file)
}
