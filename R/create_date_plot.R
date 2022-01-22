create_date_plot <-
  function(x,
           width = 450 / 72,
           height = 400 / 72,
           dpi = 72) {
    x_name <- names(x)
    plot_result <- ggplot2::ggplot(x) +
      ggplot2::geom_point(ggplot2::aes_string(x = 1:nrow(x),
                            y = x_name),
                 color = palette_colors$graylagoon$bondi) +
      ggplot2::xlab("Index") +
      theme_minimal() +
      theme(axis.title.y = ggplot2::element_blank())

    png_file <- tempfile(fileext = ".png")
    fggplot2::ggsave(
      filename = png_file,
      dev = "png",
      plot = plot_result,
      width = width,
      height = height ,
      dpi = dpi
    )
    return(png_file)
  }
