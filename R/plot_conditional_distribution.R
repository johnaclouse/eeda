#' Plot conditional distribution
#'
#' A visualization showing values from the \code{eda_var} separated in two
#' sets based on the \code{target_var}. A histogram format chosen to
#' accommodate discontinuities in data. The number of bins is determined based
#' on the data in \code{eda_var}.   Conditional views of histograms will
#' involve different binwidths. This compromise was made in recognition of the
#' need to see a more accurate view of the distribuion rather than attempt to
#' compare counts at a given position on the x-axis. The default
#' \code{confidence_level} was chosen at 0.01 to address concerns of multiple
#' search. A more supportable value should be considered based on the data.
#'
#' @param df data frame
#' @param eda_var character
#' @param target_var character name of column for conditional analysis
#' @param confidence_level numeric confidence level
#' @param width numeric width of plot in pixels
#' @param height numeric height of plot in pixels
#'
#' @return NULL. Function called for side effect of rendering exploratory data
#'   anlysis visualization
#' @export
#'
#' @examples
#' # plot_conditional_distribution()
plot_conditional_distribution <- function(df,
                                          eda_var = NULL,
                                          target_var = NULL,
                                          confidence_level = 0.99,
                                          width = 300,
                                          height = 450) {
  # binding variable just to keep R CMD Check from seeing NSE as global variables
  feature <- value <- data <- results <- bootstrap <- .data <- densities <- NULL
  CI_low <- CI_high <- outliers <- r_rank_biserial <- CI <- CI_Upper <- CI_Lower <- NULL
  n_events <- condition <- condition_value <- BS_Median <- NULL
  rowname <- negative <- positive <- observations_per_feature_level <- NULL
  n <- x_mid <- x_min <- x_max <- y_min <- y_max <- NULL

  png_file <- ""

  plot_colors <-
    c(
      "negative" = "black",
      "negative no significant difference" = "#C0C0C0",
      "positive" = "red",
      "positive no significant difference" = "#FFC0C0"
    )

  # categorical distribution ----
  if (is.factor(df[[eda_var]])) {

    columns <- c(eda_var, target_var)

    plot_data <-
      df %>%
      dplyr::select(tidyselect::any_of(columns)) %>%
      table() %>%
      as.data.frame.matrix() %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(feature = rowname,
                    negative = negative,
                    positive = positive) %>%
      dplyr::arrange(dplyr::desc(feature)) %>%
      dplyr::mutate(observations_per_feature_level = negative + positive)


    # Calculate the future positions on the x axis of each bar (left border, central position, right border)
    plot_data$x_max <-
      cumsum(plot_data$observations_per_feature_level) + 2 * c(0:(nrow(plot_data) -
                                                                    1))
    plot_data$x_min <-
      plot_data$x_max - plot_data$observations_per_feature_level
    plot_data$x_mid <- (plot_data$x_min + plot_data$x_max) / 2

    plot_data <-
      plot_data %>%
      tidyr::pivot_longer(cols = c(negative, positive),
                          names_to = "condition_value") %>%
      dplyr::mutate(
        y_min = dplyr::if_else(
          condition_value == "negative",
          0,
          1 - (value / observations_per_feature_level)
        ),
        y_max = dplyr::if_else(
          condition_value == "negative",
          value / observations_per_feature_level,
          1
        ),
      )


    log_likelihood_ratio <-
      table(
        df[[eda_var]],
        dplyr::if_else(df[target_var] == "positive", 1, 0)
      ) %>%
      DescTools::GTest()


    p_value <- log_likelihood_ratio[["p.value"]][["p.value"]]




    plot_data <- plot_data %>%
      # dplyr::rowwise() %>%
      dplyr::mutate(condition_value = factor(
        dplyr::if_else(
          condition_value == "positive",
          dplyr::if_else(
            p_value > (1 - confidence_level),
            "positive no significant difference",
            "positive"
          ),
          dplyr::if_else(
            condition_value == "negative",
            dplyr::if_else(
              p_value > (1 - confidence_level),
              "negative no significant difference",
              "negative"
            ),
            "error"
          )
        )
      ))

    x_axis_labels <- plot_data %>%
      dplyr::distinct(feature,
                      x_mid)

    plot_output <-
      ggplot2::ggplot(plot_data) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = x_min,
          xmax = x_max,
          ymin = y_min,
          ymax = y_max,
          fill = condition_value
        ),
        color = NA
      ) +
      ggplot2::scale_fill_manual(values = plot_colors) +
      ggplot2::scale_x_continuous(breaks = x_axis_labels$x_mid,
                                  labels = x_axis_labels$feature,
                                  expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0)) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        axis.text.x = ggplot2::element_text(
          size = 8,
          colour = "black",
          angle = 90,
          hjust = 0,
          vjust = 0.5
        ),
        axis.text.y = ggplot2::element_text(
          size = 8,
          colour = "black",
          hjust = 1,
          vjust = 0.3
        )) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 4))

    png_file <- tempfile(fileext = ".png")
    grDevices::png(
      png_file,
      width = width,
      height = height
    )
    print(plot_output)
    grDevices::dev.off()
  }

  # continuous distribution ----
  if (is.numeric(df[[eda_var]])) {
    bs_median <- function(x, i) {
      stats::median(x[i])
    }

    bootstrap_median <- function(x) {
      x <- x[!is.na(x)]
      bs <- boot::boot(x,
                       bs_median,
                       R = 1000
      )

      bs_ci <- boot::boot.ci(bs, type = "perc")

      if (!is.null(bs_ci)) {
        tidyr::tibble(
          BS_Median = bs_ci$t0,
          CI_Lower = bs_ci$percent[4],
          CI_Upper = bs_ci$percent[5]
        )
      } else {
        tidyr::tibble(
          BS_Median = stats::median(x),
          CI_Lower = stats::median(x),
          CI_Upper = stats::median(x)
        )
      }
    }

    long_data <-
      df %>%
      dplyr::select(tidyselect::any_of(c(target_var, eda_var))) %>%
      tidyr::pivot_longer(
        values_to = "value",
        names_to = "feature",
        cols = -tidyselect::all_of(target_var)
      ) %>%
      dplyr::select(
        condition = tidyselect::all_of(target_var),
        feature,
        value
      ) %>%
      stats::na.omit()

    effect_size_data <-
      long_data %>%
      dplyr::group_by(feature) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        results = purrr::map(
          data,
          ~ effectsize::rank_biserial(
            data = .,
            value ~ condition,
            ci = confidence_level
          )
        )
      ) %>%
      tidyr::unnest(results)

    median_data <-
      long_data %>%
      dplyr::group_by(feature, condition) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        n = purrr::map_dbl(data, nrow),
        bootstrap = purrr::map(
          .x = data,
          ~ bootstrap_median(.x$value)
        ),
        densities = purrr::map(
          .x = data,
          ~ hist(.x$value,
                 breaks = 30,
                 plot = FALSE
          )
        )
      ) %>%
      tidyr::unnest(bootstrap) %>%
      # dplyr::mutate(max_density = map_dbl(
      #   .data$densities, density_max))
      # dplyr::select(-data)
      dplyr::mutate(max_density = purrr::map_dbl(
        .data$densities, ~ max(.$density)
      )) %>%
      dplyr::select(
        -data,
        -densities
      ) %>%
      dplyr::ungroup()


    plot_data <-
      effect_size_data %>%
      tidyr::unnest(data) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(condition_value = factor(dplyr::if_else(
        condition == "positive",
        dplyr::if_else(dplyr::between(0, CI_low, CI_high) == TRUE, "positive no significant difference", "positive"),
        dplyr::if_else(
          condition == "negative",
          dplyr::if_else(dplyr::between(0, CI_low, CI_high) == TRUE, "negative no significant difference", "negative"),
          "Error"
        )
      )))


    outlier_data <- long_data %>%
      dplyr::group_by(feature, condition) %>%
      dplyr::summarize(
        outliers = list(grDevices::boxplot.stats(value)$out),
        .groups = "drop"
      ) %>%
      tidyr::unnest(outliers)


    caption_data <-
      effect_size_data %>%
      tidyr::unnest(data) %>%
      dplyr::group_by(feature, r_rank_biserial, CI, CI_low, CI_high) %>%
      dplyr::summarize(
        n = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        r_rank_biserial = round(r_rank_biserial, 2),
        CI_low = round(CI_low, 2),
        CI_high = round(CI_high, 2),
      )

    number_of_features <- length(unique(plot_data$value))
    num_bins <-
      min(
        number_of_features,
        diff(range(plot_data$value)) / (2 * stats::IQR(plot_data$value) / length(plot_data$value)^(1 / 3))
      )

    plot_output <-
      ggplot2::ggplot() +

      # histogram
      ggplot2::geom_histogram(
        data = plot_data,
        ggplot2::aes(
          x = value,
          # y = after_stat(density),
          fill = condition_value,
          group = condition_value
        ),
        bins = num_bins,
        alpha = 0.6
      ) +

      # median and confidence interval
      ggplot2::geom_rect(
        data = median_data,
        ggplot2::aes(
          xmin = CI_Lower,
          xmax = CI_Upper
        ),
        fill = "blue",
        ymin = -Inf,
        ymax = Inf,
        alpha = 0.3
      ) +
      ggplot2::geom_vline(
        data = median_data,
        ggplot2::aes(
          xintercept = BS_Median
        ),
        color = "blue",
        size = 0.75
      ) +
      {
        if (nrow(outlier_data) > 0) {
          ggplot2::geom_point(
            data = outlier_data,
            ggplot2::aes(
              x = outliers,
              y = 0,
              group = condition
            ),
            color = "blue",
            shape = 21,
            size = 2,
          )
        }
      } +
      ggplot2::geom_text(
        # data = plot_data %>% distinct(condition, feature, p_value, effsize),
        data = median_data,
        ggplot2::aes(
          x = Inf,
          y = Inf,
          label = paste("n =", scales::comma(n))
        ),
        size = 2.5,
        vjust = 1,
        hjust = 1,
        inherit.aes = FALSE
      ) +
      ggplot2::facet_wrap(
        . ~ condition,
        ncol = 1,
        labeller = ggplot2::labeller(feature = split_label),
        scales = "free_y"
      ) +
      # ggplot2::scale_x_continuous(trans = "pseudo_log", labels = scales::comma_format()) +
      ggplot2::scale_x_continuous(trans = scales::sqrt_trans(), labels = scales::comma_format()) +
      # ggplot2::scale_x_continuous(trans = "log1p", labels = scales::comma_format()) +
      # ggplot2::scale_x_continuous(labels = scales::comma_format()) +
      ggplot2::scale_color_manual(values = plot_colors) +
      ggplot2::scale_fill_manual(values = plot_colors) +
      ggplot2::labs(
        title = "Median, CI, and outliers",
        caption = glue::glue("n = {scales::comma(caption_data$n)}
                           effect size (r_rank_biserial) = {caption_data$r_rank_biserial}
                           {scales::percent(caption_data$CI)} CI \\
                           [{caption_data$CI_low}, {caption_data$CI_high}]"),
        x = NULL,
        y = "Count",
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(
        title = "Condition",
        nrow = 1
      )) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(size = 12),
        legend.position = "bottom",
        panel.spacing.y = grid::unit(0.3, "lines"),
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        strip.background = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_blank(),
        # axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(nrow = 4))

    png_file <- tempfile(fileext = ".png")
    grDevices::png(
      png_file,
      width = width,
      height = height
    )
    print(plot_output)
    grDevices::dev.off()
  }
  return(png_file)
}
