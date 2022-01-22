library(glue)
# x <- df["factor_test"]

# css ----
factor_css <- "
  <style>
    .factor-summary-table{white-space:nowrap}

    .factor-summary-table tr>td:nth-child(even) {text-align:right;}
    .factor-summary-table tr>td:nth-child(odd) {text-align:right; font-weight: bold;}
    .factor-summary-table tr:nth-child(4),
    .factor-summary-table tr:nth-child(5),
    .factor-summary-table tr:nth-child(6),
    .factor-summary-table tr:nth-child(7),
    .factor-summary-table tr:nth-child(8),
    .factor-summary-table tr:nth-child(9),
    .factor-summary-table tr:nth-child(10),
    .factor-summary-table tr:nth-child(11),
    .factor-summary-table tr:nth-child(12) {line-height:0.75;}
    .factor-summary-table tr:nth-child(4),
    .factor-summary-table tr:nth-child(10) {height:45px; vertical-align: bottom;}
  </style>
  "

create_factor_summary_table <- function (x) {
  x_table <- tidyr::as_tibble(table(x))
  x_table$x <- fct_reorder(x_table$x, x_table$n, .desc = T)
  x_table <- arrange(x_table, desc(n))
  n_xzv <-
    if_else(
      caret::nearZeroVar(x, saveMetrics = TRUE)$nzv,
      "<span style='background-color:rgba(255,0,0,0.5)'>Near-zero variance</span>",
      ""
    )
  n_x <- length(pull(x))
  n_missing <- sum(is.na(pull(x)))
  n_empty <- sum(pull(x) == "")
  empty_ratio <- n_empty / n_x
  n_nonmissing <- sum(!is.na(pull(x)))
  missing_ratio <- n_missing / n_x
  x_w <- na.omit(pull(x))
  unique_n <- length(unique(x_w))

  style <- "'
    /* table{border-collapse: separate;} */
    .factor-summary-table table {border-collapse: separate;}

    /* row 1 is for near zero variance warnings */

    /* rows */
    /* top and bottom rows spaced further apart */
    .factor-summary-table tbody>:nth-child(2),
    .factor-summary-table tbody>:nth-child(3),
    .factor-summary-table tbody>:nth-child(4) {
    line-height:1.1;
    }

    /* columns */
    /* horizontal separation for middle section */
    .factor-summary-table tbody>tr>:nth-child(3),
    .factor-summary-table tbody>tr>:nth-child(5) {
    border-left: 15px solid transparent;
    }

    /* right align values*/
    .factor-summary-table tbody>tr>:nth-child(2),
    .factor-summary-table tbody>tr>:nth-child(4),
    .factor-summary-table tbody>tr>:nth-child(6) {
    text-align:right;
    color:blue;
    }'"


  cat(
    glue(
      "<BR>",
      "<table class='table-condensed factor-summary-table'>\n",
      "  <tr>\n",
      "    <td></td>",
      "    <td></td>",
      "    <td></td>",
      "    <td colspan=2> {n_xzv} </td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td> Observations (n) </td>",
      "    <td> {curios::alignx_n(n_x)} </td>",
      "    <td> Unique (n) </td>",
      "    <td> {curios::alignx_n(unique_n)} </td>",
      "    <td></td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td> Missing (n) </td>",
      "    <td> {curios::alignx_n(n_missing)} </td>",
      "    <td> Missing ratio </td>",
      "    <td> {curios::alignx_n(missing_ratio)} </td>",
      "    <td></td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td> Empty (n) </td>",
      "    <td> {curios::alignx_n(n_empty)} </td>",
      "    <td> Empty ratio </td>",
      "    <td> {curios::alignx_n(empty_ratio)} </td>",
      "    <td></td>\n",
      "  </tr>\n",
      "</table>\n"
    )
  )
}

