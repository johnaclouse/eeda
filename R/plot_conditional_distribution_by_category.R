plot_conditional_distribution_by_category <- function(
  df,
  eda_column,
  condition_column,
  reference_column,
  width = 600,
  height = 450) {

  png_file <- ""
  columns <- c(eda_column, condition_column, reference_column)

  plot_data <-
    df %>%
    dplyr::select(tidyselect::any_of(columns))

  names(plot_data) <- c("feature_level", "target_level", "reference")

plot_output <-
  ggplot2::ggplot(plot_data) +
    ggridges::geom_density_ridges(
      ggplot2::aes(
        x = reference,
        y = feature_level,
        fill = target_level
      ),
      scale = 1,
      alpha = 0.6
    ) +
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
