create_discrete_plot <-
  function(x,
           width = 700 / 72,
           height = 400 / 72,
           dpi = 72) {
    # x = mpg["year"]
    # x[5:10,"year"]<-NA
    n <- pull(x)
    x_w <- na.omit(n)
    x_o <- boxplot.stats(x_w)$out
    x_wo <- na.omit(ifelse(x_w %in% x_o, NA, x_w))
    x_name <- stringr::str_replace_all(names(x)[1], " ", "")
    names(x)[1] <- x_name
    x$type <- "Inlier"
    x[is.na(x[[x_name]]), "type"] <- "Missing"
    x[x[[x_name]] %in% x_o, "type"] <- "Outlier"
    x$type <- factor(x$type,
                     levels = c("Missing", "Inlier", "Outlier"))

    x[[x_name]] <- factor(x[[x_name]], levels = c("Missing", unique(sort(x[[x_name]]))))

    x[is.na(x[[x_name]]), x_name] <- "Missing"


    label_interval <- ceiling(nrow(unique(x[x_name])) / 20)

    every_nth = function(n) {
      return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
    }

    plot_result <- ggplot(data = x) +
      geom_bar(
        aes_string(x = x_name,
                   fill = "type")
      ) +
      scale_fill_manual(values = c("Missing" = palette_colors$graylagoon$gray, "Inlier" = palette_colors$graylagoon$bondi, "Outlier" = palette_colors$graylagoon$plum)) +
      scale_x_discrete(breaks = every_nth(n = label_interval)) +
      theme_minimal() +
      theme(legend.title = element_blank(),
            legend.text=element_text(size=rel(1.2)))

    # plot_result <- ggplot() +
    #   geom_bar(
    #     aes(x = x_o),
    #     fill = "red"
    #   ) +
    #   geom_bar(
    #     aes(x = x_wo),
    #     fill = "#0c4c8a",
    #   ) +
    #   theme_minimal() +
    #   theme(axis.title.x = element_blank(),
    #         axis.title.y = element_blank())

    png_file <- tempfile(fileext = ".png")
    ggsave(
      filename = png_file,
      dev = "png",
      plot = plot_result,
      width = width,
      height = height ,
      dpi = dpi
    )
    return(png_file)
  }
