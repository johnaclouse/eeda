create_numeric_scatter_plot <-
  function(x,
           width = 450 / 72,
           height = 400 / 72,
           dpi = 72) {
    n <- dplyr::pull(x)
    x_w <- stats::na.omit(n)
    x_o <- grDevices::boxplot.stats(x_w)$out
    x_wo <- stats::na.omit(ifelse(x_w %in% x_o, NA, x_w))
    x_o_floor <- grDevices::boxplot.stats(x_w)$stats[1]
    x_o_ceiling <- grDevices::boxplot.stats(x_w)$stats[5]
    x_w_05 <- unname(stats::quantile(x_w, 0.05, na.rm = TRUE))
    x_w_median <- stats::median(x_w, na.rm = TRUE)
    x_w_95 <- unname(stats::quantile(x_w, 0.95, na.rm = TRUE))

    # if (length(x_w) > 100000) {
    #   temp <- round_points(x_w)
    #   # plot_data <- x_w[!duplicated(temp)]
    #   plot_data <- sample(x_w, 100000)
    #   rounded_label <- ggplot2::geom_text(ggplot2::aes(x = Inf, y = -Inf,
    #                                  hjust = 1,
    #                                  vjust = -0.1,
    #                                  label = "Points down sampled to 100,000 to reduce overplotting"))
    # } else {
    #   plot_data <- x_w
    #   rounded_label <- NULL
    # }

    plot_result <-
      ggplot2::ggplot() +
      ggrastr::rasterise(
        ggplot2::geom_jitter(
          ggplot2::aes(
            x = x_w,
            y = 1,
            size = dplyr::if_else(x_w %in% x_o, 2, 1.5),
            color = dplyr::if_else(x_w %in% x_o, palette_colors$graylagoon$plum, palette_colors$graylagoon$bondi)
          ),
          # stroke = 0.6,
          width = 0
        ),
        dpi = 300) +
      ggplot2::geom_segment(ggplot2::aes(
        x = x_o_floor,
        xend = x_o_floor,
        y = .55,
        yend = 1.475
      ),
      color = palette_colors$graylagoon$lagoon) +
      ggplot2::geom_segment(ggplot2::aes(
        x = x_w_05,
        xend = x_w_05,
        y = .55,
        yend = 1.42
      ),
      color = palette_colors$graylagoon$gray) +
      ggplot2::geom_segment(ggplot2::aes(
        x = x_w_median,
        xend = x_w_median,
        y = .55,
        yend = 1.42
      ),
      color = palette_colors$graylagoon$gray) +
      ggplot2::geom_segment(ggplot2::aes(
        x = x_w_95,
        xend = x_w_95,
        y = .55,
        yend = 1.42
      ),
      color = palette_colors$graylagoon$gray) +
      ggplot2::geom_segment(ggplot2::aes(
        x = x_o_ceiling,
        xend = x_o_ceiling,
        y = .55,
        yend = 1.475
      ),
      color = palette_colors$graylagoon$lagoon) +
      ggplot2::geom_text(
        ggplot2::aes(
          x = c(x_o_floor, x_w_05, x_w_median, x_w_95, x_o_ceiling),
          y = c(1.505, 1.45, 1.45, 1.45, 1.505),
          hjust = c(0.025, 0.4, 0.4, 0.4, 0.975),
          label = c("Lower Outlier", "5%", "50%", "95%", "Upper Outlier")
        ),
        color = c(palette_colors$graylagoon$lagoon, palette_colors$graylagoon$gray, palette_colors$graylagoon$gray, palette_colors$graylagoon$gray, palette_colors$graylagoon$lagoon)
      ) +
      # rounded_label +
      ggplot2::scale_size_identity() +
      ggplot2::scale_shape_identity() +
      ggplot2::scale_color_identity() +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank()
      ) +
      ggplot2::theme(plot.margin = grid::unit(c(5, 30, 5, 5), "pt"))

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
