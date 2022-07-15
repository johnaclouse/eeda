#' Univariate and associated bivariate EDA visualizations
#'
#' The function has several side-effects including the creation of one or more
#' plots in a temporary folder. The function is intended to be run from within a
#' R Markdown of flexdashboad document in a code chunk with results="asis"
#' @param df a dataframe
#' @param eda_var character name of the column upon which the extra exploratory
#'   data analysis will be performed
#' @param target_var character name of the column used for conditional analysis
#' @param reference_var a numeric continuous valued vector to be plotted as
#'   x-axis reference for comparison of \code{eda_var} by \code{target_var}
#' @param menu_title character title under which the individual exploratory data
#'   analysis pages will be linked in the dashboard.
#' @param discrete_cutoff numerical if the \code{eda_var} has less than
#'   \code{discrete_cutoff} distinct levels, the \code{eda_var} will be treated
#'   like a factor.
#'
#' @return returns html containing css, summaries, and graphs for included
#'   analysis.
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' # extra_eda(df = eeda::eeda_test_data, eda_var = "eg_continuous")
extra_eda <- function(df,
                      eda_var,
                      target_var = NULL,
                      reference_var = NULL,
                      menu_title = "EDA",
                      discrete_cutoff = 10) {
  add_eeda_style()

  # message("Processing extra EDA for: ", eda_var)

  cat(
    glue::glue(
      "\n\n# {eda_var} {{data-navmenu='{menu_title}'}}\n\n"
    )
  )

  data_height <- 475

  if (!is.null(target_var)) data_height <- data_height + 475

  cat("\n\n### ", eda_var, " {data-height=", data_height, "} \n\n", sep = "")

  # univariate table ----
  cat("<table>\n<tr>\n")

  if (
    is.character(df[[eda_var]]) |
    is.factor(df[[eda_var]]) |
    is.logical(df[[eda_var]])
  ) {
    render_uni_factor(df, eda_var)
  }

  if (is.numeric(df[[eda_var]])) {
    if (is_discrete(df[[eda_var]])) {
      render_uni_discrete(df, eda_var)
    } else {
      render_uni_numeric(df, eda_var)
    }
  }

  if (class(df[[eda_var]]) == "Date") render_uni_date(df, eda_var)

  cat("</tr>\n</table>\n")

  # bivariate table ----
  if (!is.null(target_var)) {
    cat("<br>")
    cat("<table>\n<tr>\n")

    if (is.factor(df[[eda_var]])) {
      render_bi_factor(df, eda_var, target_var, reference_var)
    }


    if (is.logical(df[[eda_var]])) {
      df[[eda_var]] <- factor(df[[eda_var]])
      render_bi_factor(df, eda_var, target_var, reference_var)
    }

    if (is.numeric(df[[eda_var]])) {
      if (is_discrete(df[[eda_var]], cutoff = discrete_cutoff)) {
        df[[eda_var]] <- factor(df[[eda_var]])
        render_bi_factor(df, eda_var, target_var, reference_var)
      } else {
        render_bi_continuous(df, eda_var, target_var, reference_var)
      }
    }
    cat("</tr>\n</table>\n")
    cat("<br>")
    cat("<table>\n<tr>\n")
    render_bi_cross_correlation(df, eda_var, target_var, reference_var)
    render_bi_x2y(df, eda_var, target_var, reference_var)
    render_bi_missing_by_target(df, eda_var, target_var, reference_var)
    render_bi_aggregated_missing(df, eda_var, target_var, reference_var)
    cat("</tr>\n</table>\n")
  }
}


render_uni_character <- function(df, eda_var) {
  if (length(unique(df[[eda_var]])) == length(df[[eda_var]]) |
      eda_var == "Member_Id_Universal") {
    cat("<td>Unique character or identifier column</td>\n")
  } else {
    if (stringr::str_detect(names(df[eda_var]), "_Id")) {
      cat("<td>Non-unique Identifier </td>\n")
    } else {
      if (length(unique(df[[eda_var]])) != length(df[[eda_var]])) {
        df[[eda_var]] <- factor(df[[eda_var]])
        render_uni_factor(df, eda_var)
      }
    }
  }
}


render_uni_factor <- function(df, eda_var) {
  if (stringr::str_detect(names(df[eda_var]), "_Id")) {
    cat("<td> Identifier </td>\n")
  } else {
    cat("<td style='vertical-align:top;'>\n")
    cat(create_factor_summary_table(df[eda_var]))
    cat("</td>\n")

    cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
    word_cloud_png <- create_wordcloud(df[eda_var], 125)
    if (word_cloud_png != "") {
      cat("<img src='", word_cloud_png, "'>", sep = "")
    } else {
      cat("All values present were unique")
    }
    cat("</td>\n")

    cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
    cat(create_freq_table(df[eda_var]))
    cat("</td>\n")
  }
}


render_uni_date <- function(df, eda_var) {
  cat("<td style='vertical-align:text-top;'>\n")
  cat(create_date_summary_table(df[eda_var]))
  cat("</td>\n")

  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  cat("<img src='", create_date_plot(df[eda_var]), "'>", sep = "")
  cat("</td>\n")
}


render_uni_numeric <- function(df, eda_var) {
  cat("<td style='vertical-align:top;'>\n")
  cat(create_numeric_summary_table(df[eda_var]))
  cat("</td>\n")

  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  cat("<img src='", create_numeric_scatter_plot(df[eda_var]), "'>", sep = "")
  cat("</td>\n")

  cat("<td style='vertical-align:top;'>\n")
  cat("<img src='", create_numeric_distribution_plot(df[eda_var]), "'>", sep = "")
  cat("</td>\n")

  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  cat(create_benford_plot(df[eda_var]))
  cat("</td>\n")
}



render_uni_discrete <- function(df, eda_var) {
  cat("<td style='vertical-align: text-top;'>\n")
  cat(create_discrete_summary_table(df[eda_var]))
  cat("</td>\n")

  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  cat("<img src='", create_discrete_plot(df[eda_var]), "'>", sep = "")
  cat("</td>\n")

  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  cat(unlist(create_freq_table(df[eda_var])))
  cat("</td>\n")
}




render_bi_factor <- function(df, eda_var, target_var, reference_var) {
  n <- NULL
  # div <- if (nrow(unique(x)) > 15) {
  cat("<td style='vertical-align: text-top;'>\n")
  cat("**", eda_var, " ~ ", target_var, "**\n\n", sep = "")
  cat("<div style = 'overflow-y: scroll; height:360px;'>")
  # } else {
  #   "<div>"
  # }
  html_table <-
  df %>%
    dplyr::select(tidyselect::any_of(c(eda_var, target_var))) %>%
    dplyr::group_by(dplyr::across(1:2)) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = tidyselect::any_of(target_var),
      values_from = n
    ) %>%
    dplyr::mutate({{eda_var}} := paste0("**", .[[1]], "**"),
                  spacer = "")

  html_table <- knitr::kable(html_table,
                             col.names = c("", "negative", "positive", ""),
                             format = "html",
                             escape = F,
                             align = "r") %>%
    # kableExtra::column_spec(1, width = "30em") %>%
    # kableExtra::column_spec(c(2), width_min = "3em") %>%
    kableExtra::column_spec(c(3), width_min = "5em") %>%
    kableExtra::column_spec(c(4), width_min = "0.5em")
  cat(html_table, "</div>\n")

  cat("</td>\n")
  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")

  cforest_bi_perf_plot <- plot_cforest_bi_perf(df, eda_var, target_var = target_var)
  if (cforest_bi_perf_plot != "") {
    cat("<img src='", cforest_bi_perf_plot, "'>", sep = "")
  } else {
    cat("Plot not available")
  }

  cat("</td>\n")
  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")

  conditional_distribution_plot <- plot_conditional_distribution(df, eda_var, target_var)
  if (conditional_distribution_plot != "") {
    cat("<img src='", conditional_distribution_plot, "'>", sep = "")
  } else {
    cat("Plot not available")
  }
  cat("</td>\n")

  reference_var %>%
    purrr::walk(~ render_conditional_distribution_by_category(df, eda_var, target_var, reference_var = .x))

  # cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  # conditional_distribution_by_category_plot <-
  #   plot_conditional_distribution_by_category(df, eda_var, target_var, reference_var)
  # if (conditional_distribution_by_category_plot != "") {
  #   cat("<img src='", conditional_distribution_by_category_plot, "'>", sep = "")
  # } else {
  #   cat("Plot not available")
  # }
  # cat("</td>\n")
}



render_conditional_distribution_by_category <- function(df, eda_var, target_var, reference_var) {
  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  conditional_distribution_by_category_plot <-
    plot_conditional_distribution_by_category(df, eda_var, target_var, reference_var)
  if (conditional_distribution_by_category_plot != "") {
    cat("<img src='", conditional_distribution_by_category_plot, "'>", sep = "")
  } else {
    cat("Plot not available")
  }
  cat("</td>\n")
}



render_bi_continuous <- function(df, eda_var, target_var, reference_var) {
  # R CMD check
  target <- n <- na <- p00 <- p50 <- median <- p90 <- p95 <- p100 <- NULL

  cat("<td width = '250px' style='vertical-align: text-top;'>\n")
  cat("**", eda_var, " ~ ", target_var, "**\n\n", sep = "")

  summary_table <-
    dlookr::relate(dlookr::target_by(df, target_var), eda_var) %>%
    dplyr::select(
      target,
      n,
      na,
      p00,
      mean,
      median = p50,
      p50,
      p90,
      p95,
      p100
    ) %>%
    dplyr::mutate(
      n = curios::alignx_n(n),
      na = curios::alignx_n(na),
      p00 = curios::alignx_n(curios::roundx_n(p00)),
      mean = curios::alignx_n(curios::roundx_n(mean)),
      median = curios::alignx_n(curios::roundx_n(median)),
      p90 = curios::alignx_n(curios::roundx_n(p90)),
      p95 = curios::alignx_n(curios::roundx_n(p95)),
      p100 = curios::alignx_n(curios::roundx_n(p100))
    ) %>%
    t() %>%
    .[-1,]

  row.names(summary_table) <- paste0("**", row.names(summary_table), "**")

  knitr::kable(
    summary_table,
    col.names = c("&emsp; negative", "&emsp;&emsp; positive", "&emsp;&emsp;&emsp; total &emsp;"),
    format = "html",
    escape = F,
    align = "r") %>%
    print()

  cat("</td>\n")
  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")

  cforest_bi_perf_plot <- plot_cforest_bi_perf(df, eda_var, target_var = target_var)
  if (cforest_bi_perf_plot != "") {
    cat("<img src='", cforest_bi_perf_plot, "'>", sep = "")
  } else {
    cat("Plot not available")
  }

  cat("</td>\n")
  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")

  conditional_distribution_plot <- plot_conditional_distribution(df, eda_var, target_var)
  if (conditional_distribution_plot != "") {
    cat("<img src='", conditional_distribution_plot, "'>", sep = "")
  } else {
    cat("Plot not available")
  }

  cat("</td>\n")

  reference_var %>%
    purrr::walk(~ render_conditional_feature_by_reference(df, eda_var, target_var, reference_var = .x))
}

render_conditional_feature_by_reference <- function(df, eda_var, target_var, reference_var) {
  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  plot_conditional_feature_by_reference_plot <-
    plot_conditional_feature_by_reference(df, eda_var, target_var, reference_var)
  if (plot_conditional_feature_by_reference_plot != "") {
    cat("<img src='", plot_conditional_feature_by_reference_plot, "'>", sep = "")
  } else {
    cat("Plot not available")
  }
  cat("</td>\n")
}


render_bi_cross_correlation <- function(df, eda_var, target_var, reference_var) {
  cat("<td style='padding-left: 0em; vertical-align:top;'>\n")
  cat("<img src='",
      plot_cross_correlations(df, eda_var, target_var, reference_var),
      "'>", sep = "")
  cat("</td>\n")
}

render_bi_x2y <- function(df, eda_var, target_var, reference_var) {
  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  cat("<img src='",
      plot_x2y(df, eda_var, target_var, reference_var),
      "'>", sep = "")
  cat("</td>\n")
}

render_bi_aggregated_missing <- function(df, eda_var, target_var, reference_var) {
  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  cat("<img src='",
      plot_aggregated_missing(df, eda_var, target_var, reference_var),
      "'>", sep = "")
  cat("</td>\n")
}

render_bi_missing_by_target <- function(df, eda_var, target_var, reference_var) {
  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  cat("<img src='",
      plot_missing_by_target(df, eda_var, target_var, reference_var),
      "'>", sep = "")
  cat("</td>\n")
}








