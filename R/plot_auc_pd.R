#' plot_cforest_bi_perf
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
#' # plot_conditional_distribution_by_category(df = eeda::eeda_test_data,
#' #                                          eda_var = "eg_factor_4_na",
#' #                                          target_var = "target")
plot_cforest_bi_perf <- function(df,
                        eda_var,
                        target_var,
                        width = 200,
                        height = 450) {
  png_file <- ""


  is_factor <- ifelse(is.factor(df[[eda_var]]), TRUE, FALSE)

  if (is_factor) {
    dummy_vars <- lares::ohse(df[[eda_var]],
                              redundant = TRUE,
                              limit = NA
    )
    plot_data <-
      df %>%
      dplyr::select(tidyselect::any_of(target_var)) %>%
      cbind(dummy_vars)
    eda_var <- names(dummy_vars)
  } else {
    columns <- c(target_var, eda_var)
    plot_data <-
      df %>%
      dplyr::select(tidyselect::any_of(columns))
  }

  indices <- caret::createDataPartition(plot_data[[target_var]], p = 0.7, list = FALSE)
  training_data <- plot_data[indices, c("target", eda_var)]
  testing_data <- plot_data[-indices, c("target", eda_var)]

  cforest_fit <- party::cforest(stats::as.formula(paste(target_var, " ~ .")),
                                data = training_data,
                                control = party::cforest_unbiased(
                                  mtry = floor(sqrt(length(eda_var)))
                                )
  )

  test_roc <- pROC::roc(
    response = factor(testing_data[[target_var]], ordered = TRUE),
    predictor = factor(
      stats::predict(cforest_fit,
                     newdata = testing_data[eda_var]
      ),
      ordered = TRUE
    )
  )
  test_auc <- round(pROC::auc(test_roc), 2)



  auc_plot <- pROC::ggroc(test_roc) +
    ggplot2::geom_segment(ggplot2::aes(x = 1, xend = 0, y = 0, yend = 1), color = "grey", linetype = "dashed") +
    ggplot2::annotate("text",
                      x = 0.4, y = 0.4,
                      label = paste("AUC = ", test_auc)
    ) +
    ggplot2::theme_minimal()

  if (is_factor) {
    alpha <- ifelse(test_auc > 0.5, 1, 0.1)
    vimp_plot <-
      party::varimpAUC(cforest_fit)%>%
      data.frame() %>%
      tibble::rownames_to_column() %>%
      setNames(., c("variable", "importance")) %>%
      dplyr::mutate(variable = forcats::fct_reorder(variable, importance)) %>%
      ggplot2::ggplot(
        ggplot2::aes(x = importance,
                     y = variable)
      ) +
      ggplot2::geom_point(color = "royalblue4",
                          size = 2,
                          alpha = alpha) +
      ggplot2::geom_segment(ggplot2::aes(x = 0, xend = importance,
                                         y = variable, yend = variable),
                            color = "royalblue4",
                            size = 1,
                            alpha = alpha
      ) +
      ggplot2::theme_minimal()

    plot_output <- cowplot::plot_grid(auc_plot, vimp_plot, ncol = 1)
  } else {
    alpha <- ifelse(test_auc > 0.5, 1, 0.1)
    pd_plot <- edarf::partial_dependence(cforest_fit, eda_var, interaction = FALSE) %>%
      ggplot2::ggplot(ggplot2::aes_string(x = eda_var, y = "positive")) +
      ggplot2::geom_line(alpha = alpha) +
      ggplot2::geom_point(alpha = alpha) +
      ggplot2::theme_minimal()

    plot_output <- cowplot::plot_grid(auc_plot, pd_plot, ncol = 1)
  }

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
