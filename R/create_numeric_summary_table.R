library(benford.analysis)
library(glue)

# x = mpg["model"]
# x = mpg["hwy"]
# x = df["CDR_Hypertension"]

# css ----
numeric_css <- "
  <style>
    .numeric-summary-table {white-space:nowrap}
    .numeric-summary-table tr>td:nth-child(even) {text-align:right;}
    .numeric-summary-table tr>td:nth-child(odd) {text-align:right; font-weight: bold;}
    .numeric-summary-table tr:nth-child(4),
    .numeric-summary-table tr:nth-child(5),
    .numeric-summary-table tr:nth-child(6),
    .numeric-summary-table tr:nth-child(7),
    .numeric-summary-table tr:nth-child(8),
    .numeric-summary-table tr:nth-child(9),
    .numeric-summary-table tr:nth-child(10),
    .numeric-summary-table tr:nth-child(11),
    .numeric-summary-table tr:nth-child(12) {line-height:0.75;}
    .numeric-summary-table tr:nth-child(4),
    .numeric-summary-table tr:nth-child(10) {height:45px; vertical-align: bottom;}
  </style>
  "

create_numeric_summary_table <- function(x) {
  n_xzv <-
    if_else(
      caret::nearZeroVar(x, saveMetrics = TRUE)$nzv,
      # "<span style='background-color:rgba(255,0,0,0.5)'>Near-zero variance</span>",
      "<mark>Near-zero variance</mark>",
      ""
    )
  n_x <- length(pull(x))
  n_missing <- sum(is.na(pull(x)))
  n_nonmissing <- sum(!is.na(pull(x)))
  missing_ratio <- n_missing / n_x
  x_w <- na.omit(pull(x))
  unique_n <- length(unique(x_w))
  unique_ratio <- unique_n / length(x_w)
  
  x_o <- boxplot.stats(x_w)$out
  o_n <- length(x_o)
  o_ratio <- o_n / n_nonmissing
  o_mean <- mean(x_o, na.rm = TRUE)
  x_wo <- na.omit(ifelse(x_w %in% x_o, NA, x_w))
  
  x_w_min <- min(x_w, na.rm = TRUE)
  x_w_max <- max(x_w, na.rm = TRUE)
  x_o_mean <- mean(x_o, na.rm = TRUE)
  x_w_mean <- mean(x_w, na.rm = TRUE)
  x_w_sd <- sd(x_w, na.rm = TRUE)
  x_w_cv <- x_w_mean / x_w_sd
  x_wo_mean <- mean(x_wo, na.rm = TRUE)
  x_w_median <- median(x_w, na.rm = TRUE)
  x_w_05 <- unname(quantile(x_w, 0.05, na.rm = TRUE))
  x_w_95 <- unname(quantile(x_w, 0.95, na.rm = TRUE))
  x_w_IQR <- IQR(x_w)
  
  x_o_floor <- boxplot.stats(x_w)$stats[1]
  x_o_ceiling <- boxplot.stats(x_w)$stats[5]
  x_ordered <- sort(na.omit(pull(x)))
  pctl <- rank(x_ordered) / length(x_ordered)
  outlier_floor_quantile <- paste0(str_pad(
    as.character(round(pctl[max(which(x_ordered <= x_o_floor))] * 100,
                       0)),
    width = 2,
    side = "left",
    "p"
  ),
  "%") %>%
    str_replace("p", "&numsp;")
  outlier_ceiling_quantile <-
    paste0(round(pctl[min(which(x_ordered >= x_o_ceiling))] * 100,
                 0),
           "%")
  
  if (min(x, na.rm = TRUE) > 0 & nrow(x) > 100) {
    digit_trends_1 <-
      benford(
        pull(x),
        number.of.digits = 1,
        discrete = T,
        sign = "positive"
      )
    benford_chi_1 <- chisq(digit_trends_1)$p.value
    digit_trends_2 <-
      benford(
        pull(x),
        number.of.digits = 1,
        discrete = T,
        sign = "positive"
      )
    benford_chi_2 <- chisq(digit_trends_2)$p.value
    
    benford_p <-
      if_else(benford_chi_1 < 0.05 | benford_chi_2 < 0.05,
              paste0("<mark>Benford p-values: (", round(benford_chi_1, 3), ", ", round(benford_chi_2, 3) , ")</mark>"),
              "")
  } else {
    benford_p <- ""
  }
  
  
  if (is.null(getOption("numeric_summary_css_added")) == TRUE) {
    options("numeric_summary_css_added" = TRUE)
    cat(system.file("extdata", "eeda.css", package = "eeda"))
  }
  
  # print table ----
  cat(
    glue(
      "<BR>",
      "<table class='table-condensed numeric-summary-table'>\n",
      "  <tr>\n",
      "    <td> Observations </td>",
      "    <td> {curios::alignx_n(n_x)} </td>",
      "    <td colspan=2> {n_xzv} </td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td> Nonmissing </td>",
      "    <td> {curios::alignx_n(n_nonmissing)} </td>",
      "    <td> Unique </td>",
      "    <td> {curios::alignx_n(unique_n)} </td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td> Missing </td>",
      "    <td> {curios::alignx_n(n_missing)} </td>",
      "    <td> Missing ratio </td>",
      "    <td> {curios::alignx_n(missing_ratio)} </td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td> Min </td>",
      "    <td> {curios::alignx_n(x_w_min)} </td>",
      "    <td></td>",
      "    <td></td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td> 5th %% </td>",
      "    <td> {curios::alignx_n(x_w_05)} </td>",
      "    <td> CV </td>",
      "    <td> {curios::alignx_n(x_w_cv)} </td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td> Mean </td>",
      "    <td> {curios::alignx_n(x_w_mean)} </td>",
      "    <td> Std Dev </td>",
      "    <td> {curios::alignx_n(x_w_sd)} </td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td> Median </td>",
      "    <td> {curios::alignx_n(x_w_median)} </td>",
      "    <td> IQR </td>",
      "    <td> {curios::alignx_n(x_w_IQR)} </td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td> 95th %% </td>",
      "    <td> {curios::alignx_n(x_w_95)} </td>",
      "    <td></td>",
      "    <td></td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td> Max </td>",
      "    <td> {curios::alignx_n(x_w_max)} </td>",
      "    <td></td>",
      "    <td></td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td> Outlier  </td>",
      "    <td> {curios::alignx_n(o_n)} </td>",
      "    <td> Outlier ratio </td>",
      "    <td> {curios::alignx_n(o_ratio)} </td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td> Outlier floor </td>",
      "    <td> {curios::alignx_n(x_o_floor)} </td>",
      "    <td> Out. floor %tile </td>",
      "    <td> {outlier_floor_quantile} </td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td> Outlier ceiling </td>",
      "    <td> {curios::alignx_n(x_o_ceiling)} </td>",
      "    <td> Out. ceiling %tile </td>",
      "    <td> {outlier_ceiling_quantile} </td>\n",
      "  </tr>\n",
      "  <tr>\n",
      "    <td></td>",
      "    <td></td>",
      "    <td colspan=2> {benford_p} </td> \n",
      "  </tr>\n",
      "</table>\n"
    )
  )
}
