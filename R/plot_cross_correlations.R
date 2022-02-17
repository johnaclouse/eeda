#' Plot cross correlations
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
plot_cross_correlations <- function(df,
                                    eda_var = NULL,
                                    target_var = NULL,
                                    reference_var = NULL,
                                    width = 300,
                                    height = 450) {

  png_file <- ""

  plot_colors <- c("#FC766A", "#5B84B1") # living coral, pacific coast
  # https://www.designwizard.com/blog/design-trends/colour-combination

  plot_output <-
    df %>%
    dplyr::select(dplyr::any_of(c(eda_var, target_var, reference_var))) %>%
  lares::corr_cross(
    pvalue = TRUE,
    max_pvalue = 0.01
  ) +
    ggplot2::scale_color_manual(values = c("white", "white")) +
    ggplot2::scale_fill_manual(values = plot_colors) +
    ggplot2::labs(
      title = "Cross-correlations",
      subtitle = NULL
      )


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
