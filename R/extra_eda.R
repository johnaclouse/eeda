render_character <- function(x) {
  x_name <- names(x)
  if (length(unique(x[[1]])) == length(x[[1]]) |
      names(x) == "Member_Id_Universal") {
    cat("<td> Unique identifier </td>\n")
  } else {
    if (stringr::str_detect(names(x), "_Id")) {
      cat("<td> Non-unique Identifier </td>\n")
    } else {
      if (length(unique(x[[1]])) != length(x[[1]])) {
        x[[1]] <- factor(x[[1]])
        render_factor(x)
      }
    }
  }
}


render_factor <- function(x) {
  if (stringr::str_detect(names(x), "_Id")) {
    cat("<td> Identifier </td>\n")
  } else {
    cat("<td style='vertical-align:top;'>\n")
    cat(create_factor_summary_table(x))
    cat("</td>\n")

    cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
    word_cloud_png <- create_wordcloud(x, 125)
    if (word_cloud_png != "") {
      # cat("<img src='", word_cloud_png, "' style = 'max-width: 100%; height: auto;'>", sep = "")
      # cat("<img src='", word_cloud_png, "' style = 'height: 400px;'>", sep = "")
      cat("<img src='", word_cloud_png, "'>", sep = "")
    } else {
      cat("All values present were unique")
    }
    cat("</td>\n")

    cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
    cat(create_freq_table(x))
    cat("</td>\n")
  }
}


render_date <- function(x) {
  cat("<td style='vertical-align:text-top;'>\n")
  cat(create_date_summary_table(x))
  cat("</td>\n")

  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  cat("<img src='", create_date_plot(x), "'>", sep = "")
  cat("</td>\n")
}


render_numeric <- function(x) {
  cat("<td style='vertical-align:top;'>\n")
  cat(create_numeric_summary_table(x))
  cat("</td>\n")

  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  cat("<img src='", create_numeric_scatter_plot(x), "'>", sep = "")
  cat("</td>\n")

  cat("<td style='vertical-align:top;'>\n")
  cat("<img src='", create_numeric_distribution_plot(x), "'>", sep = "")
  cat("</td>\n")

  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  cat(create_benford_plot(x))
  cat("</td>\n")
}



render_discrete <- function(x) {
  cat("<td style='vertical-align: text-top;'>\n")
  cat(create_discrete_summary_table(x))
  cat("</td>\n")

  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  cat("<img src='", create_discrete_plot(x), "'>", sep = "")
  cat("</td>\n")

  cat("<td style='padding-left: 1em; vertical-align:top;'>\n")
  cat(unlist(create_freq_table(x)))
  cat("</td>\n")
}

#' A conglomeration of univariate and bivariate EDA visualizations
#'
#' The function has several side-effects including the creation of one or more
#' plots in a temporary folder. The function is intended to be run from within a
#' R Markdown of flexdashboad document in a code chunk with results="asis"
#' @param x a dataframe
#' @param include_univariate logical
#' @param bivariate_categorical_target logical
#' @param bivariate_continuous_target logical
#'
#' @return returns html containing css, summaries, and graphs for included analysis.
#' @export
#'
#' @examples
#' # conglomerate_eda(extra_eda::extra_eda_test_data["eg_continuous"])
extra_eda <- function(x,
                             include_univariate = TRUE,
                             bivariate_categorical_target = NULL,
                             bivariate_continuous_target = NULL) {
  add_extra_eda_style()
  x_name <- names(x)
  message("Conglomerating EDA for:", x_name)

    cat("\n\n###", x_name, " {data-height=500} \n\n")

  # univariate table
  if (include_univariate) {
    cat("<table>\n<tr>\n")

    if (is.character(x[[1]])) render_character(x)
    if (is.factor(x[[1]])) render_factor(x)
    if (class(x[[1]]) == "Date") render_date(x)
    if (is.numeric(x[[1]])) {
      if (is_discrete(x[[1]])) {
        render_discrete(x)
      } else {
        render_numeric(x)
      }
    }
    cat("</tr>\n</table>\n")
  }
}

