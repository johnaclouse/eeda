# setup ----
n_rows <- 2000
set.seed(51)

create_events <- function(number_of_individuals = 100,
                          number_of_days = 365,
                          events_per_year = c(
                            1 / 365,
                            2.5 / 365,
                            12 / 365,
                            52 / 365
                          ),
                          event_weights = c(
                            0.15,
                            0.78,
                            0.06,
                            0.01
                          ),
                          visit_cost = c(150, 1500, 15000, 150000),
                          visit_weights = c(.8, .09, .009, 0.001)) {
  events <- data.frame(
    id = as.numeric(),
    day = as.numeric(),
    monetary = as.numeric()
  )
  for (i in 1:number_of_individuals) {
    rate <- sample(events_per_year, 1, prob = event_weights)
    for (j in 1:number_of_days) {
      if (rpois(1, rate) > 0) {
        new_event <-
          data.frame(
            id = i,
            day = j,
            monetary = sample(visit_cost, 1, prob = visit_weights) *
              (1 + rnorm(1) / 10)
          )
        events <- rbind(events, new_event)
      }
    }
  }
  return(events)
}



# events ----
events <-
  dplyr::left_join(
    tibble::tibble(id = 1:n_rows),
    create_events(n_rows) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(
        recency = min(day),
        frequency = dplyr::n(),
        monetary = round(sum(monetary))
      ),
    by = "id"
  ) %>%
  dplyr::distinct(
    id,
    recency,
    frequency,
    monetary
  ) %>%
  dplyr::mutate(rfm = efe::quantile_of_quantiles(., c("recency", "frequency", "monetary")))

# data set ----
eeda_test_data <- tidyr::tibble(
  target = as.numeric(NA),
  key_age = round(rlnorm(n = n_rows, meanlog = 3.5, sdlog = .34), 1),
  key_recency = events$recency,
  key_frequency = events$frequency,
  key_monetary = events$monetary,
  key_rfm = events$rfm,
  key_is_left_handed = as.integer(sample(c(0, 1), n_rows, replace = TRUE, prob = c(0.9, 0.1))),
  key_role = factor(sample(c("rouleur", "grimpeur", "puncheur", "sprinteurs", "domestique", "gc"), n_rows, replace = TRUE, prob = c(1, 3, 1, 1, 4, 1))),
  eg_factor_4 = factor(sample(c("dragon", "fish", "raccoon", "dog"), n_rows, replace = TRUE, prob = c(0.3, 0.3, 0.3, 0.1))),
  eg_factor_4_na = eg_factor_4,
  eg_factor_12 = factor(sample(colors()[37:48], n_rows, replace = TRUE)),
  eg_factor_12_na = eg_factor_12,
  eg_factor_50 = factor(sample(state.name, n_rows, replace = TRUE)),
  eg_factor_50_na = eg_factor_50,
  eg_binary = as.integer(sample(c(0, 1), n_rows, replace = TRUE)),
  eg_binary_na = eg_binary,
  eg_logical = sample(c(FALSE, TRUE), n_rows, replace = TRUE),
  eg_logical_na = eg_logical,
  eg_continuous = as.numeric(NA),
  eg_continuous_na = eg_continuous,
  eg_cluster = sample(10*(1:7), n_rows, replace = TRUE) + runif(n_rows),
  eg_cluster_na = eg_cluster,
  eg_integer_7 = as.integer(sample(c(47, 53, 59, 61, 67, 71, 73), n_rows, replace = TRUE)),
  eg_integer_7_na = eg_integer_7,
  eg_integer_50 = as.integer(sample(1:50, n_rows, replace = TRUE)),
  eg_integer_50_na = eg_integer_50,
  eg_character = sample(ids::adjective_animal(n = 1500, style = "title"), n_rows, replace = TRUE),
  eg_character_na = eg_character,
  eg_long_character = purrr::map_chr(1:n_rows, ~ paste(stringr::str_split(stringi::stri_rand_lipsum(1, FALSE), " ")[[1]][1:4], collapse = " ")),
  eg_long_character_na = eg_long_character,
  eg_date = Sys.Date() - rpois(n = n_rows, lambda = 1) * 100,
  eg_date_na = eg_date
)


# modify data ----
eeda_test_data <- eeda_test_data %>%
  dplyr::mutate(
    # create associations between variables
    # eg_factor_4 = factor(dplyr::if_else(key_age > 50 & runif(1) > 0.3, "dragon", as.character(eg_factor_4))),
    # eg_factor_4 = factor(dplyr::if_else(target == "positive" & runif(1) > 0.2, "dragon", as.character(eg_factor_4))),
    eg_continuous = runif(n_rows) * 100 + 10 * pmin(key_age, 45)^.53,

    # create target relationships
    target = runif(n = n_rows),
    target = target / 2,
    target = target + (key_age - 50) / 100,
    target = target + dplyr::if_else(runif(n_rows) > 0.4, eg_continuous / 2000, 0),
    target = target + dplyr::if_else(runif(n_rows) > 0.5 & eg_factor_4 == "dragon", 0.2, 0),
    target = factor(dplyr::if_else(target > 0.5, "positive", "negative")),
    # add NA values
    eg_continuous_na = replace(eg_continuous, sample(1:n_rows, 200), NA),
    eg_cluster_na = replace(eg_cluster, sample(1:n_rows, 200), NA),
    eg_factor_4_na = replace(eg_factor_4, sample(1:nrow(eeda_test_data), 200), NA),
    eg_factor_12_na = replace(eg_factor_12, sample(1:nrow(eeda_test_data), 200), NA),
    eg_factor_50_na = replace(eg_factor_50, sample(1:nrow(eeda_test_data), 200), NA),
    eg_binary_na = replace(eg_binary, sample(1:nrow(eeda_test_data), 200), NA),
    eg_integer_7_na = replace(eg_integer_7, sample(1:nrow(eeda_test_data), 200), NA),
    eg_integer_50_na = replace(eg_integer_50, sample(1:nrow(eeda_test_data), 200), NA),
    eg_character_na = replace(eg_character, sample(1:nrow(eeda_test_data), 200), NA),
    eg_long_character_na = replace(eg_long_character, sample(1:nrow(eeda_test_data), 200), NA),
    eg_date_na = replace(eg_date, sample(1:nrow(eeda_test_data), 200), NA)
  )



usethis::use_data(eeda_test_data, overwrite = TRUE)


# x = (eeda_test_data[["eg_cluster"]])
# suggest_number_of_clusters(eeda_test_data["eg_cluster"], diagnostic_file_prefix = "foo")
# ?kmeans
#
# require(graphics)
#
# # a 2-dimensional example
# x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
#            matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
# colnames(x) <- c("x", "y")
# (cl <- kmeans(x, 5))
# plot(x, col = cl$cluster)
# points(cl$centers, col = 1:2, pch = 8, cex = 2)
#
# # sum of squares
# ss <- function(x) sum(scale(x, scale = FALSE)^2)
#
# ## cluster centers "fitted" to each obs.:
# fitted.x <- fitted(cl);  head(fitted.x)
# resid.x <- x - fitted(cl)
#
# ## Equalities : ----------------------------------
# cbind(cl[c("betweenss", "tot.withinss", "totss")], # the same two columns
#       c(ss(fitted.x), ss(resid.x),    ss(x)))
# stopifnot(all.equal(cl$ totss,        ss(x)),
#           all.equal(cl$ tot.withinss, ss(resid.x)),
#           ## these three are the same:
#           all.equal(cl$ betweenss,    ss(fitted.x)),
#           all.equal(cl$ betweenss, cl$totss - cl$tot.withinss),
#           ## and hence also
#           all.equal(ss(x), ss(fitted.x) + ss(resid.x))
# )
#
# kmeans(x,1)$withinss # trivial one-cluster, (its W.SS == ss(x))
#
# ## random starts do help here with too many clusters
# ## (and are often recommended anyway!):
# (cl <- kmeans(x, 5, nstart = 25))
# plot(x, col = cl$cluster)
# points(cl$centers, col = 1:5, pch = 8)
# cut(x)
#
#
#
# hclust
# x = dplyr::arrange((eeda_test_data["eg_cluster"]), eg_cluster)
# foo(x)
#
# foo <- function(x,
#                                        k_limit = 10,
#                                        diagnostic_file_prefix = NULL) {
#
#   if (!is.null(diagnostic_file_prefix))
#     stopifnot("A diagnostic file prefix was specified in suggest_number_of_clusters, but the /temp directory does not exist." = dir.exists("temp"))
#
#   if(sum(is.na(x)) > 0) {
#     message(glue::glue("NA values in {names(x)} encountered and removed in suggest_number_of_clusters()"))
#     x <- stats::na.omit(x)
#   }
#
#   x <- unlist(x)
#
#   max_number_of_clusters <- pmin(length(unique(x)), k_limit)
#
#   # wss <- (nrow(x) - 1) * var(x)
#   # # wss[1] is set above when wss is created
#   # for (i in 2:max_number_of_clusters) {
#   #   wss[i] <- sum(kmeans(x, centers = i)$withinss)
#   # }
#
#   dist_x <- dist(x)
#
#   wss <- numeric(max_number_of_clusters)
#   for (i in 1:max_number_of_clusters) {
#     # wss[i] <- sum(stats::kmeans(x, centers = i)$withinss)
#     # wss[i] <- stats::kmeans(x, centers = i)$tot.withinss
#
#     k_result <- stats::kmeans(x, centers = i, nstart = 25, iter.max = 15)
#     wss[i] <- cluster::silhouette(k_result$cluster, dist_x)
#   }
#
#   distance_from_diagonal <- numeric(max_number_of_clusters)
#
#   for (i in 1:max_number_of_clusters) {
#     distance_from_diagonal[i] <- abs(scale_min_max(1:max_number_of_clusters)[i] - scale_min_max(wss[1:max_number_of_clusters])[i])
#   }
#
#   k_best <- which.min(distance_from_diagonal)
#
#     graphics::plot(scale_min_max(1:max_number_of_clusters), scale_min_max(wss[1:max_number_of_clusters]), col = "gray60", cex = 0, type = "b", xlab = NA, ylab = NA)
#     graphics::text(scale_min_max(1:max_number_of_clusters), scale_min_max(wss[1:max_number_of_clusters]), col = "royalblue4", labels = 1:max_number_of_clusters)
#     graphics::abline(0, 1, col = "gray60")
#
#   return(k_best)
# }
#
# factoextra::fviz_nbclust(x, kmeans, method = "wss")
# factoextra::fviz_nbclust(x, kmeans, method = "silhouette")
#
# pam_limited <- function(x,k) list(cluster = cluster::pam(x,k, cluster.only=TRUE))
#
#
# gap_stat <- cluster::clusGap(x, FUN = kmeans, K.max= max_number_of_clusters , B = 50)
# q = factoextra::fviz_gap_stat(gap_stat)
#
#
# plot(q)
# q$
# plot(hclust(dist(x)))
#
#
#
#
# z = eeda_test_data[["eg_cluster"]]
# y = eeda_test_data[["eg_cluster"]]
#
# plot(density(y))
# plot(y)
# plot(sort(y))
#
# x <- c(rnorm(50, sd=0.3), rnorm(50, mean=1, sd=0.3), rnorm(50, mean=2, sd=0.3))
# k <- 8 # Divide x into 3 clusters
#
# result <- Ckmeans.1d.dp::Ckmeans.1d.dp(x, k)
# result <- Ckmeans.1d.dp::Ckmeans.1d.dp(y, k)
# colors <- RColorBrewer::brewer.pal(k, "Dark2")
# plot(result, col.clusters = colors)
#
#
#
# plot(sort(y))
# hist(y)
# # Divide x into k clusters, k automatically selected (default: 1~9)
# result <- Ckmeans.1d.dp::Ckmeans.1d.dp(y)
# k <- max(result$cluster)
# colors <- brewer.pal(k, "Dark2")
# plot(result, col.clusters = colors)
#
# library(Ckmeans.1d.dp)
#
# # Ex. 1 The number of clusters is provided.
# # Generate data from a Gaussian mixture model of three components
# x = sort(y)
# x <- c(rnorm(50, sd=0.2), rnorm(50, mean=1, sd=0.3), rnorm(100,
#                                                            mean=-1, sd=0.25))
# plot(x)
# hist(x)
# # Divide x into 3 clusters
# k <- 3
#
# result <- Ckmedian.1d.dp(x)
#
# plot(result, main="Optimal univariate k-median given k")
#
# result <- Ckmeans.1d.dp(x, k)
#
# plot(result, main="Optimal univariate k-means given k")
#
# plot(x, col=result$cluster, pch=result$cluster, cex=1.5,
#      main="Optimal univariate k-means clustering given k",
#      sub=paste("Number of clusters given:", k))
# abline(h=result$centers, col=1:k, lty="dashed", lwd=2)
# legend("bottomleft", paste("Cluster", 1:k), col=1:k, pch=1:k,
#        cex=1.5, bty="n")
#
# # Ex. 2 The number of clusters is determined by Bayesian
# #       information criterion
# # Generate data from a Gaussian mixture model of three components
# x <- c(rnorm(50, mean=-3, sd=1), rnorm(50, mean=0, sd=.5),
#        rnorm(50, mean=3, sd=1))
# # Divide x into k clusters, k automatically selected (default: 1~9)
#
# result <- Ckmedian.1d.dp(x)
#
# plot(result, main="Optimal univariate k-median with k estimated")
#
# result <- Ckmeans.1d.dp(x)
#
# plot(result, main="Optimal univariate k-means with k estimated")
#
# k <- max(result$cluster)
# plot(x, col=result$cluster, pch=result$cluster, cex=1.5,
#      main="Optimal univariate k-means clustering with k estimated",
#      sub=paste("Number of clusters is estimated to be", k))
# abline(h=result$centers, col=1:k, lty="dashed", lwd=2)
# legend("topleft", paste("Cluster", 1:k), col=1:k, pch=1:k,
#        cex=1.5, bty="n")
#
# # Ex. 3 Segmenting a time course using optimal weighted
# #       univariate clustering
# n <- 160
# t <- seq(0, 2*pi*2, length=n)
# n1 <- 1:(n/2)
# n2 <- (max(n1)+1):n
# y1 <- abs(sin(1.5*t[n1]) + 0.1*rnorm(length(n1)))
# y2 <- abs(sin(0.5*t[n2]) + 0.1*rnorm(length(n2)))
# y <- c(y1, y2)
#
# w <- y^8 # stress the peaks
# res <- Ckmeans.1d.dp(t, k=c(1:10), w)
# plot(res)
# plot(t, w, main = "Time course weighted k-means",
#      col=res$cluster, pch=res$cluster,
#      xlab="Time t", ylab="Transformed intensity w",
#      type="h")
# abline(v=res$centers, col="chocolate", lty="dashed")
# text(res$centers, max(w) * .95, cex=0.5, font=2,
#      paste(round(res$size / sum(res$size) * 100), "/ 100"))
# [Package Ckmeans.1d.dp version 4.3.4 Index]
#
#
#
# BAMMtools::getJenksBreaks(y,9)
#
#
