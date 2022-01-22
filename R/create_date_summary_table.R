# set.seed(47)
# x = data.frame(test_data = Sys.Date() + c(-100:100))
# x[sample(c(1:200),20),1] <- NA

# css ----
date_css <- "
  <style>
    .date-summary-table{white-space:nowrap}

    /* table{border-collapse: separate;} */
    .date-summary-table table{border-collapse: separate;}

    /* rows */
    /* min through max stats spaced closer together */
    .date-summary-table tbody>:nth-child(3),
    .date-summary-table tbody>:nth-child(4),
    .date-summary-table tbody>:nth-child(5),
    .date-summary-table tbody>:nth-child(6),
    .date-summary-table tbody>:nth-child(7) {line-height:0.75;}

    .date-summary-table tr:nth-child(3) {height:45px; vertical-align: bottom;}

    /* columns */
    /* horizontal separation for middle section */
    .date-summary-table tbody>tr>:nth-child(3) {border-left: 40px solid transparent;}

    /* bold labels*/
    .date-summary-table tbody>tr>:nth-child(1),
    .date-summary-table tbody>tr>:nth-child(3),
    .date-summary-table tbody>tr>:nth-child(5) {text-align:left; font-weight:bold;}

    /* right align values*/
    .date-summary-tabletbody>tr>:nth-child(2),
    .date-summary-table tbody>tr>:nth-child(4),
    .date-summary-table tbody>tr>:nth-child(6) {text-align:right;}

  </style>
  "

# summerize_date ----
create_date_summary_table <- function (x) {
  n_x <- length(dplyr::pull(x))
  n_missing <- sum(is.na(dplyr::pull(x)))
  n_nonmissing <- sum(!is.na(dplyr::pull(x)))
  missing_ratio <- n_missing / n_x
  x_w <- na.omit(dplyr::pull(x))
  unique_n <- length(unique(x_w))
  x_w_min <- min(x_w, na.rm = TRUE)
  x_w_max <- max(x_w, na.rm = TRUE)
  x_w_mean <- mean(x_w, na.rm = TRUE)
  x_w_median <- mean(x_w, na.rm = TRUE)

  if (is.null(getOption("date_summary_css_added")) == TRUE) {
    options("date_summary_css_added" = TRUE)
    cat(date_css)
  }

  # print table ----
  cat(
    glue::glue(
      "<BR>",
      "<table class='table-condensed date-summary-table'>\n",
      "<tr>\n",
      "<th> Observations </th> <th> {curios::alignx_n(n_x)} </th> <th> Unique values </th> <th> {curios::alignx_n(unique_n)} </th>\n",
      "</tr>\n",
      "<tr>\n",
      "<td> Nonmissing values  </td> <td> {curios::alignx_n(n_nonmissing)} </td>",
      "<td> Missing values </td> <td> {curios::alignx_n(n_missing)} </td>",
      "<td> Missing ratio </td> <td> {curios::alignx_n(missing_ratio)} </td>\n",
      "</tr>\n",
      "<tr>\n",
      "<td> Min </td> <td> {x_w_min} </td> <td></td> <td></td>\n",
      "</tr>\n",
      "<tr>\n",
      "<td> Mean </td> <td> {x_w_mean}  </td> <td></td> <td></td>\n",
      "</tr>\n",
      "<tr>\n",
      "<td> Median </td><td> {x_w_median} </td> <td></td> <td></td>\n",
      "</tr>\n",
      "<tr>\n",
      "<td> Max </td> <td> {x_w_max} </td> <td></td> <td></td>\n",
      "</tr>\n",
      "</table>\n"
    )
  )
}
