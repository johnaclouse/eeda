create_benford_plot <- function(x,
                                width = 300 / 72,
                                height = 400 / 72,
                                dpi = 72) {
  if (min(x, na.rm = TRUE) > 0 & nrow(x) > 100) {
    trends_1 <-
      benford(
        pull(x),
        number.of.digits = 1,
        discrete = TRUE,
        sign = "both"
      )
    
    single_digit <- ggplot() +
      geom_line(aes(
        x = trends_1$bfd$digits,
        y = trends_1$bfd$benford.dist.freq
      ),
      color = "blue") +
      geom_line(aes(x = trends_1$bfd$digits,
                    y = trends_1$bfd$data.dist.freq),
                color = palette_colors$graylagoon$tangerine) +
      scale_x_continuous(n.breaks = 10) +
      labs(x = "Digit", y = "Frequency") +
      theme_minimal() +
      theme(plot.margin = unit(c(5, 30, 5, 5), "pt"))
    
    trends_2 <-
      benford(
        pull(x),
        number.of.digits = 2,
        discrete = TRUE,
        sign = "positive"
      )
    
    two_digits <- ggplot() +
      geom_line(aes(
        x = trends_2$bfd$digits,
        y = trends_2$bfd$benford.dist.freq
      ),
      color = "blue") +
      geom_line(aes(x = trends_2$bfd$digits,
                    y = trends_2$bfd$data.dist.freq),
                color = palette_colors$graylagoon$tangerine) +
      scale_x_continuous(breaks = seq(10, 100, 20)) +
      labs(x = "Digit", y = "Frequency") +
      theme_minimal() +
      theme(plot.margin = unit(c(5, 30, 5, 5), "pt"))
    
    plot_result <- single_digit + two_digits
    
    png_file <- tempfile(fileext = ".png")
    ggsave(
      filename = png_file,
      dev = "png",
      plot = plot_result,
      width = width,
      height = height ,
      dpi = dpi
    )
    
    png_file <- paste0("<img src='", png_file , "'>")
  } else {
    warning("Data does not support Benford analysis")
    png_file <- ""
  }
  return(png_file)
}
