create_freq_table <- function(x,
                              max_lines = 1000) {
  # fct_infreq(x[[1]]) %>%
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

  result <- list()
  div <- if (nrow(unique(x)) > 15) {
    "<div style = 'overflow-y: scroll; height:360px;'>"
  } else {
    "<div>"
  }

  style <-  "<style>
  .summary_freq_table {white-space:nowrap}
  .summary_freq_table { border: 10px; width: 300px; height:180px;}
  .summary_freq_table tr>td:first-child {width: 200px;}
  .summary_freq_table tr>td:last-child {width: 40px; padding-right: 40px;}
  .summary_freq_table tr>th:last-child {width: 40px; padding-right: 40px;}
  .summary_freq_table th {position: sticky; top: 0; background: white;}
  </style>"

  result$css <- paste(div, style)

  x_name <- names(x)
  html_table <- na.omit(x) %>%
    dplyr::group_by(across(all_of(x_name))) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::mutate(Proportion = round(n / sum(n), 2)) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::slice(1:max_lines)

  names(html_table) <- c("Value", "n", "Proportion")
  html_table <- knitr::kable(html_table) %>%
    kableExtra::row_spec(0, align = "c") %>%
    kableExtra::column_spec(1, width = "30em") %>%
    kableExtra::column_spec(c(2,3), width = "10em")

  result$table <- paste(html_table, "</div>\n")
  return(result)
}

