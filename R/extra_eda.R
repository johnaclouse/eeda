#' Univariate and associated bivariate EDA visualizations
#'
#' The function has several side-effects including the creation of one or more
#' plots in a temporary folder. The function is intended to be run from within a
#' R Markdown of flexdashboad document in a code chunk with results="asis"
#' @param df a dataframe
#' @param eda_var character name of the column upon which the extra
#'   exploratory data analysis will be performed
#' @param condition_var character name of the column used for conditional
#'   analysis
#' @param reference_var a numeric continuous valued vector to be plotted as
#'   x-axis reference for comparison of \code{eda_var} by
#'   \code{condition_var}
#' @param discrete_cutoff numerical if the \code{eda_var} has less than
#'   \code{discrete_cutoff} distinct levels, the \code{eda_var} will be
#'   treated like a factor.
#'
#' @return returns html containing css, summaries, and graphs for included
#'   analysis.
#' @export
#'
#' @examples
#' # extra_eda(df = eeda::eeda_test_data, eda_var = "eg_continuous")
extra_eda <- function(df,
                      eda_var,
                      condition_var = NULL,
                      reference_var = NULL,
                      discrete_cutoff = 10) {
  add_eeda_style()

  message("Processing extra EDA for: ", eda_var)

  cat("\n\n###", eda_var, " {data-height=475} \n\n")

  # univariate table ----
  cat("<table>\n<tr>\n")

  if (is.character(df[[eda_var]])) render_uni_character(df, eda_var)
  if (is.factor(df[[eda_var]])) render_uni_factor(df, eda_var)
  if (is.logical(df[[eda_var]])) render_uni_factor(df, eda_var)

  if (class(df[[eda_var]]) == "Date") render_uni_date(df, eda_var)
  if (is.numeric(df[[eda_var]])) {
    if (is_discrete(df[[eda_var]])) {
      render_uni_discrete(df, eda_var)
    } else {
      render_uni_numeric(df, eda_var)
    }
  }
  cat("</tr>\n</table>\n")

  # bivariate table ----
  if (!is.null(condition_var)) {
    cat("\n### {data-height=475}\n\n")
    cat("**", eda_var, " ~ ", condition_var, "**\n\n", sep = "")

    cat("<table>\n<tr>\n")

    if (is.factor(df[[eda_var]])) {
      render_bi_factor(df, eda_var, condition_var, reference_var)
    }


    if (is.logical(df[[eda_var]])) {
      df[[eda_var]] <- factor(df[[eda_var]])
      render_bi_factor(df, eda_var, condition_var, reference_var)
    }

    if (is.numeric(df[[eda_var]])) {
      if (is_discrete(df[[eda_var]], cutoff = discrete_cutoff)) {
        df[[eda_var]] <- factor(df[[eda_var]])
        render_bi_factor(df, eda_var, condition_var, reference_var)
      } else {
        render_bi_continuous(df, eda_var, condition_var, reference_var)
      }
    }
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




render_bi_continuous <- function(df, eda_var, condition_var, reference_var) {
  # R CMD check
  target <- n <- na <- p00 <- p50 <- p90 <- p95 <- p100 <- NULL

  cat("<td style='vertical-align: text-top;'>\n")

  dlookr::relate(dlookr::target_by(df, condition_var), eda_var) %>%
    dplyr::select(
      target,
      n,
      na,
      p00,
      mean,
      median = p50,
      p90,
      p95,
      p100
    ) %>%
    t() %>%
    knitr::kable() %>%
    print()
  cat("</td>\n")

  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  conditional_distribution_plot <- plot_conditional_distribution(df, eda_var, condition_var)
  if (conditional_distribution_plot != "") {
    cat("<img src='", conditional_distribution_plot, "'>", sep = "")
  } else {
    cat("Plot not available")
  }
  cat("</td>\n")

  reference_var %>%
    purrr::walk(~ render_conditional_feature_by_reference(df, eda_var, condition_var, reference_var = .x))
}

render_conditional_feature_by_reference <- function(df, eda_var, condition_var, reference_var) {
  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  plot_conditional_feature_by_reference_plot <-
    plot_conditional_feature_by_reference(df, eda_var, condition_var, reference_var)
  if (plot_conditional_feature_by_reference_plot != "") {
    cat("<img src='", plot_conditional_feature_by_reference_plot, "'>", sep = "")
  } else {
    cat("Plot not available")
  }
  cat("</td>\n")
}


render_bi_factor <- function(df, eda_var, condition_var, reference_var) {
  cat("<td style='vertical-align: text-top;'>\n")
  dlookr::relate(dlookr::target_by(df, condition_var), eda_var) %>%
    t() %>%
    knitr::kable() %>%
    print()
  cat("</td>\n")

  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  conditional_distribution_plot <- plot_conditional_distribution(df, eda_var, condition_var)
  if (conditional_distribution_plot != "") {
    cat("<img src='", conditional_distribution_plot, "'>", sep = "")
  } else {
    cat("Plot not available")
  }
  cat("</td>\n")

  reference_var %>%
    purrr::walk(~ render_conditional_distribution_by_category(df, eda_var, condition_var, reference_var = .x))


  # cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  # conditional_distribution_by_category_plot <-
  #   plot_conditional_distribution_by_category(df, eda_var, condition_var, reference_var)
  # if (conditional_distribution_by_category_plot != "") {
  #   cat("<img src='", conditional_distribution_by_category_plot, "'>", sep = "")
  # } else {
  #   cat("Plot not available")
  # }
  # cat("</td>\n")
}



render_conditional_distribution_by_category <- function(df, eda_var, condition_var, reference_var) {
  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  conditional_distribution_by_category_plot <-
    plot_conditional_distribution_by_category(df, eda_var, condition_var, reference_var)
  if (conditional_distribution_by_category_plot != "") {
    cat("<img src='", conditional_distribution_by_category_plot, "'>", sep = "")
  } else {
    cat("Plot not available")
  }
  cat("</td>\n")
}

