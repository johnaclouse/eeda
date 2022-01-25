create_freq_table <- function(x,
                              max_lines = 1000) {
  # binding variable just to keep R CMD Check from seeing NSE as global variables
  n <- NULL

  div <- if (nrow(unique(x)) > 15) {
    "<div style = 'overflow-y: scroll; height:360px;'>"
  } else {
    "<div>"
  }

  x_name <- names(x)
  html_table <- stats::na.omit(x) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(x_name))) %>%
    dplyr::summarize(n = dplyr::n()) %>%
    dplyr::mutate(Proportion = round(n / sum(n), 2)) %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::slice(1:max_lines) %>%
    dplyr::mutate(spacer = "")

  html_table <- knitr::kable(html_table,
                             col.names = c("Value", "n", "Proportion", "")) %>%
    kableExtra::row_spec(0, align = "r") %>%
    # kableExtra::column_spec(1, width = "30em") %>%
    kableExtra::column_spec(c(2), width_min = "4em") %>%
    kableExtra::column_spec(c(3), width_min = "6em") %>%
    kableExtra::column_spec(c(4), width_min = "1em")

  cat(div, html_table, "</div>\n")
}
