create_freq_table <- function(x,
                              max_lines = 1000) {
  # binding variable just to keep R CMD Check from seeing NSE as global variables
  n <- NULL

  # forcats::fct_infreq(x[[1]]) %>%
  #   table()  %>%
  #   as_tibble() %>%
  #   DT::datatable(
  #     rownames = FALSE,
  #     colnames = NULL,
  #     height = 300,
  #     options = list(
  #       dom = "ft",
  #       autoWidth = TRUE,
  #       pageLength = -1,
  #       lengthChange = FALSE,
  #       scrollY = '300px',
  #       filter = "top"
  #     )
  #   ) %>%
  #   htmltools::tagList()

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
    dplyr::slice(1:max_lines)

  names(html_table) <- c("Value", "n", "Proportion")
  html_table <- knitr::kable(html_table) %>%
    kableExtra::row_spec(0, align = "c") %>%
    kableExtra::column_spec(1, width = "30em") %>%
    kableExtra::column_spec(c(2,3), width = "10em")

  result$table <- paste(div, html_table, "</div>\n")
  return(result)
}

