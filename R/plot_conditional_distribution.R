is_discrete <- function(x, cutoff = 100) {
  stopifnot("x is not numeric" = is.numeric(x))
  unique_value_count <- length(unique(stats::na.omit(x)))
  ifelse(unique_value_count < cutoff, TRUE, FALSE)
}

# Setting bins at 30, no optimal value dplyr::across disparate data sets and discrete
# count numbers become hard to view as the number of bins increases
# Zero values are removed to reduce scale issues with remaining data
# Conditional views of histograms will involve different binwidth.
# Bin size effects counts - making histograms ineffective for comparing numbers
# Magnitude differences number of cases make distributions difficult to compare
# in histograms using count
# density plots are not intended for discrete value
# Can accept single feature or multiple features
# sig_value defaults to 0.01 to address some concerns encountered with multiple comparison.
plot_conditional_distribution <- function(df,
                                          condition = NULL,
                                          alpha = 0.01,
                                          conf_level = 0.95) {
  colors <-
    c(
      "Negative" = "black",
      "Negative no significant difference" = "#C0C0C0",
      "Positive" = "red",
      "Positive no significant difference" = "#FFC0C0"
    )


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
        median = bs_ci$t0,
        ci_lower = bs_ci$percent[4],
        ci_upper = bs_ci$percent[5]
      )
    } else {
      tidyr::tibble(
        median = stats::median(x),
        ci_lower = stats::median(x),
        ci_upper = stats::median(x)
      )
    }
  }


  long_data <-
    df %>%
    tidyr::pivot_longer(
      values_to = "value",
      names_to = "feature",
      cols = -all_of(condition)
    ) %>%
    dplyr::select(
      condition = all_of(condition),
      feature,
      value
    ) %>%
    stats::na.omit() %>%
    dplyr::mutate(condition = dplyr::case_when(
      condition == "Pos" ~ "Positive",
      condition == "Neg" ~ "Negative",
      TRUE ~ "Unknown"
    ))



  # stats ----
  # wilcox_data <-
  #   long_data %>%
  #   group_by(feature) %>%
  #   nest() %>%
  #   dplyr::mutate(
  #     results = map(
  #       data,
  #       ~ rstatix::wilcox_effsize(
  #         data = .,
  #         value ~ condition
  #       )
  #     ),
  #     p_value = map_dbl(
  #       data,
  #       ~ wilcox.test(data = ., value ~ condition)$p.value
  #     )
  #   )

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
          ci = conf_level
        )
      )
    ) %>%
    tidyr::unnest(results)



  median_data <-
    long_data %>%
    dplyr::group_by(feature, condition) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      n = map_dbl(data, nrow),
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
    dplyr::mutate(Conditional_Value = factor(dplyr::if_else(
      condition == "Positive",
      dplyr::if_else(between(0, CI_low, CI_high) == TRUE, "Positive no significant difference", "Positive"),
      dplyr::if_else(
        condition == "Negative",
        dplyr::if_else(between(0, CI_low, CI_high) == TRUE, "Negative no significant difference", "Negative"),
        "Error"
      )
    )))


  outlier_data <- long_data %>%
    dplyr::group_by(feature, condition) %>%
    dplyr::summarize(outliers = list(grDevices::boxplot.stats(value)$out),
              .groups = "drop") %>%
    tidyr::unnest(outliers)


  caption_data <-
    effect_size_data %>%
    tidyr::unnest(data) %>%
    dplyr::group_by(feature, r_rank_biserial, CI, CI_low, CI_high) %>%
    dplyr::summarize(n = dplyr::n(),
              .groups = "drop") %>%
    dplyr::mutate(r_rank_biserial = round(r_rank_biserial, 2),
           CI_low = round(CI_low, 2),
           CI_high = round(CI_high, 2),
    )

  number_of_features <- length(unique(plot_data$value))
  num_bins <-
    min(
      number_of_features,
      diff(range(plot_data$value)) / (2 * stats::IQR(plot_data$value) / length(plot_data$value)^(1 / 3))
    )





  ggplot2::ggplot() +



    # histogram ----
  ggplot2::geom_histogram(
    data = plot_data,
    ggplot2::aes(
      x = value,
      # y = after_stat(density),
      fill = Conditional_Value,
      group = Conditional_Value
    ),
    bins = num_bins,
    alpha = 0.6
  ) +
    ggplot2::geom_rect(
      data = median_data,
      ggplot2::aes(
        xmin = ci_lower,
        xmax = ci_upper
      ),
      fill = "blue",
      ymin = -Inf,
      ymax = Inf,
      alpha = 0.3
    ) +
    ggplot2::geom_vline(
      data = median_data,
      ggplot2::aes(
        xintercept = median
      ),
      color = "blue",
      size = 0.75
    ) +


    {
      if (nrow(outlier_data) > 0) {
        ggplot2::geom_point(
          data = outlier_data,
          ggplot2::aes(x = outliers,
              y = 0,
              group = condition),
          color = "blue",
          shape = 21,
          size = 2,

        )

      }

    } +

    # ggplot2::geom_boxplot(
    #   data = plot_data,
    #   ggplot2::aes(
    #     x = value,
    #     group = condition
    #   ),
    #   outlier.color = "blue",
    #   outlier.fill = NA,
    #   outlier.shape = 21,
    #   outlier.size = 2,
  #   # fill = NA,
  #   # color = NA,
  #   notch = TRUE
  # ) +

  # labels ----
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
    facet_wrap(
      . ~ condition,
      ncol = 1,
      labeller = labeller(feature = split_label),
      scales = "free_y"
    ) +
    # ggplot2::scale_x_continuous(trans = "pseudo_log", labels = scales::comma_format()) +
    ggplot2::scale_x_continuous(trans = sqrt_trans(), labels = scales::comma_format()) +
    # ggplot2::scale_x_continuous(trans = "log1p", labels = scales::comma_format()) +
    # ggplot2::scale_x_continuous(labels = scales::comma_format()) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::labs(
      title = glue::glue(
        "{caption_data$feature} ~ {condition} (Positive or Negative)
        Density plot with median confidence intervals and outliers by target state"),
      caption = glue::glue("n = {scales::comma(caption_data$n)}   \\
                           effect size (r_rank_biserial) = {caption_data$r_rank_biserial}  \\
                           {scales::percent(caption_data$CI)} CI \\
                           [{caption_data$CI_low}, {caption_data$CI_high}]"),
      x = NULL,
      y = "Count",
    ) +
    guides(fill = guide_legend(
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
    )
}



# df <-
#   data$engineered %>%
#   dplyr::select(
#     data$target$name,
#     LN_SDOH_AddrStability,
#     LN_SDOH_WealthIndex,
#     LN_SDOH_ProspectBankingExperience,
#     LN_SDOH_EducationProgram4Yr,
#     LN_SDOH_Motivation_Level,
#     LN_SDOH_Medication_Adherence_Rate,
#     LN_SDOH_Readmission_Probability,
#     LN_SDOH_Socio_Index,
#     LN_SDOH_Socio_Rank,
#     any_of(data$key_measures),
#   ) %>%
#   dplyr::mutate(Sex = dplyr::if_else(Sex == "M", 1, 0))

# plot_conditional_distribution(
#   df = df,
#   condition = data$target$name)
#


# plot_conditional_distribution(df = df[c(data$target$name, names(df)[10])], condition = data$target$name)





# q <-
#   data$engineered %>%
#   dplyr::select(
#     data$target$name,
#     # LN_SDOH_AddrStability,
#     LN_SDOH_WealthIndex
#     # LN_SDOH_ProspectBankingExperience,
#     # LN_SDOH_EducationProgram4Yr,
#     # Chronic_Condition_Cnt,
#     # ED_Visits_L12M,
#     # Admits_L12M,
#     # Readmits_L12M,
#     # Sex
#   ) %>%
#   dplyr::mutate(dplyr::across(where(is.numeric), factor))
#
# names(q)[-1] %>%
#   walk(~ hist(q[[.x]], main  = .x))
# names(q)[-1] %>%
#   purrr::map(~ length(unique(q[[.x]])))
# names(q) %>%
#   purrr::map(~ levels(q[[.x]]))
#
#
# condition <- data$target$name
#
# long_data <-
#   q %>%
#   tidyr::pivot_longer(
#     values_to = "value",
#     names_to = "feature",
#     cols = -all_of(condition)
#   ) %>%
#   dplyr::select(
#     condition = all_of(condition),
#     feature,
#     value
#   ) %>%
#   stats::na.omit() %>%
#   dplyr::mutate(condition = case_when(
#     condition == "Pos" ~ "Positive",
#     condition == "Neg" ~ "Negative",
#     TRUE ~ "Unknown"
#   ))
#
# chi_square <- chisq.test(table(q[[2]], dplyr::if_else(q[1] == "Pos", 1, 0)))
#
# caption_data <-
#
#   n <- stats::na.omit(nrow(q))
#
# ggplot2::ggplot(long_data,
#        ggplot2::aes(x = value,
#            fill = condition),
# ) +
#   ggplot2::geom_bar(alpha = dplyr::if_else(chi_square$p.value < alpha, 1, 0.3)) +
#   # facet_wrap(~ feature,
#   #            scales = "free") +
#
#   facet_grid(condition ~ feature,
#              scales = "free") +
#   ggplot2::scale_y_continuous(labels = scales::comma) +
#
#   ggplot2::labs(
#     caption = glue::glue("n = {scales::comma(n)}   \\
#                            chi-square p-value = {curios::roundx_n(chi_square$p.value)}"),
#     x = NULL,
#     y = NULL,
#   ) +
#   ggplot2::theme_minimal()
#
#
# feature_by_target <- dlookr::target_by(q, data$target$name)
#
# # also works with numerical features
#
# # ADD THIS TO THE BIVARIATE VIEW
#
# # variable US        n    na  mean    sd se_mean   IQR skewness
# # <chr>    <fct> <int> <int> <dbl> <dbl>   <dbl> <dbl>    <dbl>
# #   1 Sales    No      142     0  6.82  2.60   0.218  3.44   0.323
# # 2 Sales    Yes     258     0  7.87  2.88   0.179  4.23   0.0760
# # 3 Sales    total   400     0  7.50  2.82   0.141  3.93   0.186
# dlookr::relate(feature_by_target, "LN_SDOH_WealthIndex")
#
# library(dlookr)
# summary(dlookr::relate(feature_by_target, "LN_SDOH_WealthIndex"))
# plot(dlookr::relate(feature_by_target, "LN_SDOH_WealthIndex"), typographic = T)
# plot(dlookr::relate(feature_by_target, "LN_SDOH_WealthIndex"), typographic = T) +
#   # ggtitle("Foo") +
#   ggplot2::labs(caption = "My p-value",
#        ) +
#   ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = 0.5),
#         axis.title.y = ggplot2::element_text(hjust = 0.5)) +
#   ggplot2::scale_fill_manual(values = c("red", "green", "blue"))
#
