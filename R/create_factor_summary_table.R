create_factor_summary_table <- function (x) {
  # binding variable just to keep R CMD Check from seeing NSE as global variables
  n <- NULL
  x_table <- tidyr::as_tibble(table(x))
  x_table$x <- forcats::fct_reorder(x_table$x, x_table$n, .desc = T)
  x_table <- dplyr::arrange(x_table, dplyr::desc(n))
  n_xzv <-
    dplyr::if_else(
      caret::nearZeroVar(x, saveMetrics = TRUE)$nzv,
      "<span style='background-color:rgba(255,0,0,0.5)'>Near-zero variance</span>",
      ""
    )
  n_x <- length(dplyr::pull(x))
  n_missing <- sum(is.na(dplyr::pull(x)))
  n_empty <- sum(dplyr::pull(x) == "")
  empty_ratio <- n_empty / n_x
  n_nonmissing <- sum(!is.na(dplyr::pull(x)))
  missing_ratio <- n_missing / n_x
  x_w <- stats::na.omit(dplyr::pull(x))
  unique_n <- length(unique(x_w))

  cat(
    glue::glue(
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
