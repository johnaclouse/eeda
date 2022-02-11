#' plot_conditional_distribution_by_category
#'
#' @param df data frame
#' @param eda_var character
#' @param target_var character name of column for conditional analysis
#' @param reference_var character vector with the names of reference columns for conditional plotting
#' @param width numeric width of plot in pixels
#' @param height numeric height of plot in pixels
#'
#' @return NULL. Function called for side effect of rendering exploratory data
#'   anlysis visualization
#' @export
#'
#' @examples
#' # plot_conditional_distribution_by_category(df = eeda::eeda_test_data[1:2000,],
#' #                                          eda_var = names(eeda::eeda_test_data)[24],
#'  #                                          eda_var = names(eeda::eeda_test_data)[8],
#' #                                          target_var = "target",
#' #                                          reference_var = "key_age")
plot_conditional_distribution_by_category <- function(df,
                                                      eda_var,
                                                      target_var,
                                                      reference_var,
                                                      width = 250,
                                                      height = 450) {
  cluster <- value <- feature_level <- target_level <- reference <- NULL

  png_file <- ""
  columns <- c(eda_var, target_var, reference_var)

  plot_data <-
    df %>%
    dplyr::select(tidyselect::any_of(columns))

  if (is.numeric(plot_data[[eda_var]])) {
    if (length(unique(plot_data[[eda_var]])) > 25)
      stop(glue::glue("in plot_conditional_distribution_by_category: \\
                        too many unique values in {eda_var} to plot as a factor.
                        {length(unique(plot_data[[eda_var]]))} unique values found, but only 25 are allowed."))
    plot_data[[eda_var]] <- factor(plot_data[[eda_var]],
                                   exclude = NULL,
                                   levels = sort(unique(plot_data[[eda_var]]), na.last = TRUE))
  }

  plot_output <-
    plot_data %>%
    ggplot2::ggplot(
      ggplot2::aes_string(
        x = reference_var,
        y = eda_var,
        fill = target_var
      )
    ) +
  ggridges::geom_density_ridges(
    scale = 1,
    alpha = 0.6
  ) +
    ggplot2::scale_fill_manual(values = c("red", "black")) +
    ggplot2::labs(x = reference_var, y = eda_var) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")


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
