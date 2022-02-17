#' plot_missing_by_target
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
plot_missing_by_target <- function(df,
                                    eda_var = NULL,
                                    target_var = NULL,
                                    reference_var = NULL,
                                    plot_colors = c("present" = "#949398", "missing" = "#F4DF4E"), # Ultimate Gray, Illuminating
                                    width = 300,
                                    height = 450) {
  # binding variable just to keep R CMD Check from seeing NSE as global variables
  is_missing <- NULL

  png_file <- ""

  plot_output <-
    df %>%
    dplyr::select(dplyr::any_of(c(target_var, eda_var))) %>%
    dplyr::mutate(is_missing = factor(dplyr::if_else(is.na(dplyr::across(eda_var)), "missing", "present"))) %>%
    dplyr::group_by(dplyr::across(target_var), is_missing) %>%
    dplyr::summarise(count = dplyr::n()) %>%

    ggplot2::ggplot(
      ggplot2::aes_string(x = target_var,
                          y = "count",
                          fill = "is_missing"
      )
    ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "fill"
    ) +
    ggplot2::scale_fill_manual(values = plot_colors) +
    ggplot2::labs(y = glue::glue("percent of {eda_var} missing")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank())

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
