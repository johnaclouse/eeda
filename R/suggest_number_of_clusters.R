#' suggest_number_of_clusters
#'
#' Algorithm establishes the maximum number of cluster based on the lesser of
#' k_limit and the number of unique values in x. A set of kmeans models are
#' created starting with a single cluster and progressing to the maximum number
#' of clusters. For model, the sum of within sum of squares is calculated. Note
#' that kmeans model produces a within sum of squares for k (number of clusters)
#' = 1. If the method is changed from kmeans, it may be necessary to create the
#' sum of squares for k = 1 manually using \eqn{degrees of freedom * sample variance}.
#'
#' Both sets of values are scaled from 0 to 1 so that the intersection may be
#' found with the line y = x. The intersection is designated as the knee of the
#' curve commonly used to determine the optimal number of clusters. The distance
#' of each point from the line y = x is calculated and the point closest to the
#' line chosen as the suggested number of clusters.
#'
#' A diagnostic plot may be produced showing the within sum of squares and
#' cluster number.
#'
#' @param x vector of numeric values
#' @param k_limit numeric maximum number of clusters to consider
#' @param diagnostic_file_prefix character, if present, a file is output with the wss~cluster number plot.
#'   number:wss curve and y = x line.
#'
#' @return numeric
#' @export
#'
#' @examples
#' # suggest_number_of_clusters()
suggest_number_of_clusters <- function(x,
                                       k_limit = 10,
                                       diagnostic_file_prefix = NULL) {

  if (!is.null(diagnostic_file_prefix))
    stopifnot("A diagnostic file prefix was specified in suggest_number_of_clusters, but the /temp directory does not exist." = dir.exists("temp"))

  if(sum(is.na(x)) > 0) {
    message(glue::glue("NA values in {names(x)} encountered and removed in suggest_number_of_clusters()"))
    x <- stats::na.omit(x)
  }

  max_number_of_clusters <- pmin(nrow(unique(x)), k_limit)

  # wss <- (nrow(x) - 1) * var(x)
  # # wss[1] is set above when wss is created
  # for (i in 2:max_number_of_clusters) {
  #   wss[i] <- sum(kmeans(x, centers = i)$withinss)
  # }

  wss <- numeric(max_number_of_clusters)
  for (i in 1:max_number_of_clusters) {
    wss[i] <- sum(stats::kmeans(x, centers = i)$withinss)
  }

  distance_from_diagonal <- numeric(max_number_of_clusters)

  for (i in 1:max_number_of_clusters) {
    distance_from_diagonal[i] <- abs(scale_min_max(1:max_number_of_clusters)[i] - scale_min_max(wss[1:max_number_of_clusters])[i])
    distance_from_diagonal[i] <- abs(scale_min_max(log(1:max_number_of_clusters))[i] - scale_min_max(log(wss[1:max_number_of_clusters]))[i])
  }

  k_best <- which.min(distance_from_diagonal)

  if(!is.null(diagnostic_file_prefix)) {
    png_file <- glue::glue("temp/{diagnostic_file_prefix}-{k_best}.png")
    grDevices::png(png_file)
    graphics::plot(scale_min_max(1:max_number_of_clusters), scale_min_max(wss[1:max_number_of_clusters]), col = "gray60", cex = 0, type = "b", xlab = NA, ylab = NA)
    graphics::text(scale_min_max(1:max_number_of_clusters), scale_min_max(wss[1:max_number_of_clusters]), col = "royalblue4", labels = 1:max_number_of_clusters)
    graphics::abline(0, 1, col = "gray60")
    grDevices::dev.off()
  }

  return(k_best)
}




# x = eeda::eeda_test_data["eg_continuous"]
# x = eeda::eeda_test_data["eg_cluster"]
#
#
# x =density(unlist(x))
#
#
# x =density(unlist(x))
#
#
# localMaxima <- function(x) {
#   # Use -Inf instead if x is numeric (non-integer)
#   y <- diff(c(-.Machine$integer.max, x)) > 0L
#   rle(y)$lengths
#   y <- cumsum(rle(y)$lengths)
#   y <- y[seq.int(1L, length(y), 2L)]
#   if (x[[1]] == x[[2]]) {
#     y <- y[-1]
#   }
#   y
# }
#
# localMinima <- function(x) {
#   # Use -Inf instead if x is numeric (non-integer)
#   y <- diff(c(.Machine$integer.max, x)) < 0L
#   rle(y)$lengths
#   y <- cumsum(rle(y)$lengths)
#   y <- y[seq.int(1L, length(y), 2L)]
#   if (x[[1]] == x[[2]]) {
#     y <- y[-1]
#   }
#   y
# }
#
# plot(x$y)
# localMaxima(x$y)
# localMinima(x$y)
#
# dx=c(0,sign(diff(x$y)))
# numberofzeros= length(dx) - sum(abs(dx)) -1 # to find the number of zeros
# # in the dx minus the first one
# # which is added intentionally.
# #running recursive formula to clear middle zeros
# # iterate for the number of zeros
# for (i in 1:numberofzeros){
#   dx = sign(2*dx + c(0,rev(sign(diff(rev(dx))))))
# }
#
# plot(x)
# points(which(diff(dx)==2),x[which(diff(dx)==2)],col = 'blue')#Local MIN.
# points(which(diff(dx)==-2),x[which(diff(dx)==-2)],col = 'red')#Local MAX.
#
#
# plot(x$y)
# pracma::findpeaks(x$y, zero = "0", peakpat = NULL,
#           minpeakheight = -Inf, minpeakdistance = 1, threshold = 0, npeaks = 0, sortstr = FALSE)
#
#
#
# x = eeda_test_data["eg_cluster"]
# x = eeda::eeda_test_data["eg_continuous"]
# x = eeda::eeda_test_data["eg_cluster"]
# x =density(unlist(x))
#
# pSignal = x$y
# plot(pSignal, type="l", col="navy")
# grid()
# x <- pracma::findpeaks(pSignal, zero = "0",peakpat = NULL, npeaks=0, threshold=0, sortstr=TRUE)
# points(x[, 2], x[, 1], pch=20, col="maroon")
#
#
# x = eeda::eeda_test_data["eg_continuous"]
# x = eeda_test_data["eg_cluster"]
# x =density(unlist(x))
# hist(eeda::eeda_test_data[["eg_cluster"]])
# dat = x$y
#
# p <- pracma::findpeaks(dat)
# v <- pracma::findpeaks(-dat)
# plot(dat, t = "l", xaxt = "n")
# points(p[,2], p[,1], pch = "+", col = "red")
# points(v[,2], -v[,1], pch = "o", col = "blue")
# axis(1, at=1:512, labels=x$x)
#
#
#
# hist(unlist(x))
# plot()
# q = hclust(dist(x))
#
# z = cutree(q, k = 3)
# plot(q)
# gap <- cluster::clusGap(x, FUN = kmeans, K.max = 10, B = 50)
# factoextra::fviz_gap_stat(gap)
#
# dend <- as.dendrogram(q)
# z = dendextend::get_branches_heights(dend)
# plot(dend)
#
#
# graphics::plot(scale_min_max(1:max_number_of_clusters), scale_min_max(log(wss[1:max_number_of_clusters])), col = "gray60", cex = 0, type = "b", xlab = NA, ylab = NA)
# graphics::text(scale_min_max(1:max_number_of_clusters), scale_min_max(log(wss[1:max_number_of_clusters])), col = "royalblue4", labels = 1:max_number_of_clusters)
# graphics::abline(0, 1, col = "gray60")
# hist(unlist(x))
