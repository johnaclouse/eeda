create_date_summary_table <- function (x) {
  n_x <- length(dplyr::pull(x))
  n_missing <- sum(is.na(dplyr::pull(x)))
  n_nonmissing <- sum(!is.na(dplyr::pull(x)))
  missing_ratio <- n_missing / n_x
  x_w <- stats::na.omit(dplyr::pull(x))
  unique_n <- length(unique(x_w))
  x_w_min <- min(x_w, na.rm = TRUE)
  x_w_max <- max(x_w, na.rm = TRUE)
  x_w_mean <- mean(x_w, na.rm = TRUE)
  x_w_median <- stats::median(x_w, na.rm = TRUE)

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
