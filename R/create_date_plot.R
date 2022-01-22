create_date_plot <-
  function(x,
           width = 450 / 72,
           height = 400 / 72,
           dpi = 72) {
    x_name <- names(x)
    plot_result <- ggplot(x) +
      geom_point(aes_string(x = 1:nrow(x),
                            y = x_name),
                 color = palette_colors$graylagoon$bondi) +
      xlab("Index") +
      theme_minimal() +
      theme(axis.title.y = element_blank())
    
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
