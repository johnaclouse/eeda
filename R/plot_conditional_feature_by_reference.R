#' plot_conditional_feature_by_reference
#'
#' @param df data frame
#' @param eda_var character
#' @param condition_var character name of column for conditional analysis
#' @param reference_var character vector with the names of reference columns for conditional plotting
#' @param width numeric width of plot in pixels
#' @param height numeric height of plot in pixels
#'
#' @return NULL. Function called for side effect of rendering exploratory data
#'   anlysis visualization
#' @export
#'
#' @examples
#' # plot_conditional_distribution_by_category(df = eeda::eeda_test_data[1:200,],
#' #                                          eda_var = names(eeda::eeda_test_data)[5],
#' #                                          condition_var = "target",
#' #                                          reference_var = "key_age")
plot_conditional_feature_by_reference <- function(df,
                                                      eda_var,
                                                      condition_var,
                                                      reference_var,
                                                      width = 600,
                                                      height = 450) {
  cluster <- value <- feature_level <- target_level <- reference <- NULL

  png_file <- ""
  columns <- c(eda_var, condition_var, reference_var)

  plot_data <-
    df %>%
    dplyr::select(tidyselect::any_of(columns))

  plot_output <-
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes_string(
        x = reference_var,
        y = eda_var,
        color = condition_var,
        fill = condition_var
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth() +
    ggplot2::scale_color_manual(values = c("red", "black")) +
    ggplot2::scale_fill_manual(values = c("red", "black")) +
    ggplot2::theme_minimal()

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
