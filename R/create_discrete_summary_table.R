create_discrete_summary_table <- function(x) {
  # x = df["CDR_RA"]
  x_nzv <-
    dplyr::if_else(
      caret::nearZeroVar(x, saveMetrics = TRUE)$nzv,
      # "<span style='background-color:rgba(255,0,0,0.5)'>Near-zero variance</span>",
      "<mark>Near-zero variance</mark>",
      ""
    )
  n_x <- length(dplyr::pull(x))
  n_missing <- sum(is.na(dplyr::pull(x)))
  n_nonmissing <- sum(!is.na(dplyr::pull(x)))
  missing_ratio <- n_missing / n_x
  x_w <- stats::na.omit(dplyr::pull(x))
  x_w_mean <- mean(x_w, na.rm = TRUE)
  values <- unique(dplyr::pull(x))
  unique_n <- length(unique(x_w))
  n_1 <- sum(x_w == values[1], na.rm = TRUE)
  n_2 <- sum(x_w == values[2], na.rm = TRUE)
  n_NA <- sum(is.na(dplyr::pull(x)))
  p_1 <- n_1 / n_x
  p_2 <- n_2 / n_x
  p_NA <- n_NA / n_x

  cat(
    glue::glue(
      "<BR> <table class='table-condensed discrete-summary-table'>",
      "<tr>\n",
      "<td> Observations </td>",
      "<td> {curios::alignx_n(n_x)} </td>",
      "<td colspan=2> {x_nzv} </td>\n",
      "</tr>\n",
      "<td> Nonmissing </td> <td> {curios::alignx_n(n_nonmissing)} </td> <td> Unique </td> <td> {curios::alignx_n(unique_n)} </td>",
      "</tr>",
      "<tr>",
      "<td> Missing </td> <td> {curios::alignx_n(n_missing)} </td> <td> Missing ratio </td> <td> {curios::alignx_n(missing_ratio)} </td>",
      "</tr>",
      "</table>",
      .sep = "\n"
    )
  )
}
