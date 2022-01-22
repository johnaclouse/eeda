library(patchwork)
create_numeric_distribution_plot <-
  function(x,
           width = 450 / 72,
           height = 400 / 72,
           dpi = 72) {
    n <- pull(x)
    x_w <- na.omit(n)
    x_o <- boxplot.stats(x_w)$out
    x_wo <- na.omit(ifelse(x_w %in% x_o, NA, x_w))
    x_o_floor <- boxplot.stats(x_w)$stats[1]
    x_o_ceiling <- boxplot.stats(x_w)$stats[5]
    x_w_05 <- unname(quantile(x_w, 0.05, na.rm = TRUE))
    x_w_median <- mean(x_w, na.rm = TRUE)
    x_w_95 <- unname(quantile(x_w, 0.95, na.rm = TRUE))
    hist_w <- ggplot() +
      aes(x = x_w) +
      geom_density(fill = "#0c4c8a") +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    box_w <- ggplot() +
      geom_boxplot(
        aes(x = x_w),
        fill = palette_colors$graylagoon$lagoon,
        color = palette_colors$graylagoon$lagoon,
        outlier.size = 2,
        outlier.shape = 16,
        outlier.color = palette_colors$graylagoon$plum,
        # stroke = 1,
        notch = TRUE
      ) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    box_wo <- ggplot() +
      geom_boxplot(
        aes(x = x_wo),
        fill = palette_colors$graylagoon$bondi,
        color = palette_colors$graylagoon$bondi,
        alpha = 0.6,
        notch = TRUE
      ) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    hist_w <- ggplot() +
      aes(x = x_w) +
      geom_density(fill = palette_colors$graylagoon$lagoon) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    hist_wo <- ggplot() +
      aes(x = x_wo) +
      geom_density(fill = palette_colors$graylagoon$bondi,
                   alpha = 0.6, ) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    plot_result <- (box_w / hist_w) | (box_wo / hist_wo)
    
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
