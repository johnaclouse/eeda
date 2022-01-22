create_discrete_plot <-
  function(x,
           width = 700 / 72,
           height = 400 / 72,
           dpi = 72) {
    # x = mpg["year"]
    # x[5:10,"year"]<-NA
    n <- dplyr::pull(x)
    x_w <- stats::na.omit(n)
    x_o <- grDevices::boxplot.stats(x_w)$out
    x_wo <- stats::na.omit(ifelse(x_w %in% x_o, NA, x_w))
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

    plot_result <- ggplot2::ggplot(data = x) +
      ggplot2::geom_bar(
        ggplot2::aes_string(x = x_name,
                   fill = "type")
      ) +
      ggplot2::scale_fill_manual(values = c("Missing" = palette_colors$graylagoon$gray, "Inlier" = palette_colors$graylagoon$bondi, "Outlier" = palette_colors$graylagoon$plum)) +
      ggplot2::scale_x_discrete(breaks = every_nth(n = label_interval)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.title = ggplot2::element_blank(),
            legend.text=ggplot2::element_text(size = ggplot2::rel(1.2)))

    # plot_result <- ggplot2::ggplot() +
    #   ggplot2::geom_bar(
    #     ggplot2::aes(x = x_o),
    #     fill = "red"
    #   ) +
    #   ggplot2::geom_bar(
    #     ggplot2::aes(x = x_wo),
    #     fill = "#0c4c8a",
    #   ) +
    #   ggplot2::theme_minimal() +
    #   ggplot2::theme(axis.title.x = ggplot2::element_blank(),
    #         axis.title.y = ggplot2::element_blank())

    png_file <- tempfile(fileext = ".png")
    ggplot2::ggsave(
      filename = png_file,
      dev = "png",
      plot = plot_result,
      width = width,
      height = height ,
      dpi = dpi
    )
    return(png_file)
  }
