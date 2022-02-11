#' plot_auc_pd
#'
#' Generates a single plot composed of an receiver operator chracteristic
#' sensitivity:specificity plot and partial dependency plot. The output is
#' intended to help characterize the ability of the \code{eda_var} to predict
#' the \code{target_var}
#'
#' @param df data frame
#' @param eda_var character
#' @param target_var character name of column for conditional analysis
#' @param width numeric width of plot in pixels
#' @param height numeric height of plot in pixels
#'
#' @return character path to the resulting png file
#' @export
#'
#' @examples
#' # plot_conditional_distribution_by_category(df = eeda::eeda_test_data,
#' #                                          eda_var = "eg_continuous",
#' #                                          target_var = "target")
plot_auc_pd <- function(df,
                        eda_var,
                        target_var,
                        width = 200,
                        height = 450) {


  png_file <- ""
  columns <- c(target_var, eda_var)

  plot_data <-
    df %>%
    dplyr::select(tidyselect::any_of(columns)) %>%
    stats::na.omit()

  indices <- caret::createDataPartition(plot_data[[target_var]], p = 0.7, list = FALSE)
  training_data <- plot_data[indices, ]
  testing_data <- plot_data[-indices, ]

  cforest_fit <- party::cforest(stats::as.formula(paste(target_var, " ~ ", eda_var)),
                                data = training_data,
                                control = party::cforest_unbiased(mtry = 1))

  test_roc <- pROC::roc(
    response = factor(testing_data[[target_var]], ordered = TRUE),
    predictor = factor(
      predict(cforest_fit,
              newdata = testing_data[eda_var]),
      ordered = TRUE)
  )
  test_auc <- round(pROC::auc(test_roc), 2)

  auc_plot <- pROC::ggroc(test_roc) +
    ggplot2::geom_segment(ggplot2::aes(x = 1, xend = 0, y = 0, yend = 1), color="grey", linetype="dashed") +
    ggplot2::annotate("text", x = 0.4, y = 0.4,
                      label = paste("AUC = ", test_auc)) +
    ggplot2::theme_minimal()


  alpha <- ifelse(test_auc > 0.5, 1, 0.1)
  pd_plot <- edarf::partial_dependence(cforest_fit, eda_var, data = training_data[eda_var]) %>%
    ggplot2::ggplot(ggplot2::aes_string(x = eda_var, y = "positive")) +
    ggplot2::geom_line(alpha = alpha) +
    ggplot2::geom_point(alpha = alpha) +
    ggplot2::theme_minimal()

  plot_output <- cowplot::plot_grid(auc_plot, pd_plot, ncol = 1)

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
