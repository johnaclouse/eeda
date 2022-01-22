library(patchwork)
create_numeric_distribution_plot <-
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
    x_w_median <- mean(x_w, na.rm = TRUE)
    x_w_95 <- unname(stats::quantile(x_w, 0.95, na.rm = TRUE))
    hist_w <- ggplot2::ggplot() +
      ggplot2::aes(x = x_w) +
      ggplot2::geom_density(fill = "#0c4c8a") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank())
    
    box_w <- ggplot2::ggplot() +
      ggplot2::geom_boxplot(
        ggplot2::aes(x = x_w),
        fill = palette_colors$graylagoon$lagoon,
        color = palette_colors$graylagoon$lagoon,
        outlier.size = 2,
        outlier.shape = 16,
        outlier.color = palette_colors$graylagoon$plum,
        # stroke = 1,
        notch = TRUE
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank())
    
    box_wo <- ggplot2::ggplot() +
      ggplot2::geom_boxplot(
        ggplot2::aes(x = x_wo),
        fill = palette_colors$graylagoon$bondi,
        color = palette_colors$graylagoon$bondi,
        alpha = 0.6,
        notch = TRUE
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank())
    
    hist_w <- ggplot2::ggplot() +
      ggplot2::aes(x = x_w) +
      ggplot2::geom_density(fill = palette_colors$graylagoon$lagoon) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank())
    
    hist_wo <- ggplot2::ggplot() +
      ggplot2::aes(x = x_wo) +
      ggplot2::geom_density(fill = palette_colors$graylagoon$bondi,
                   alpha = 0.6, ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank())
    
    plot_result <- (box_w / hist_w) | (box_wo / hist_wo)
    
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
