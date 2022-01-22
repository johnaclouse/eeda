create_benford_plot <- function(x,
                                width = 300 / 72,
                                height = 400 / 72,
                                dpi = 72) {
  if (min(x, na.rm = TRUE) > 0 & nrow(x) > 100) {
    trends_1 <-
      benford.analysis::benford(
        dplyr::pull(x),
        number.of.digits = 1,
        discrete = TRUE,
        sign = "both"
      )

    single_digit <- ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(
        x = trends_1$bfd$digits,
        y = trends_1$bfd$benford.dist.freq
      ),
      color = "blue") +
      ggplot2::geom_line(ggplot2::aes(x = trends_1$bfd$digits,
                    y = trends_1$bfd$data.dist.freq),
                color = palette_colors$graylagoon$tangerine) +
      ggplot2::scale_x_continuous(n.breaks = 10) +
      ggplot2::labs(x = "Digit", y = "Frequency") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.margin = grid::unit(c(5, 30, 5, 5), "pt"))

    trends_2 <-
      benford.analysis::benford(
        dplyr::pull(x),
        number.of.digits = 2,
        discrete = TRUE,
        sign = "positive"
      )

    two_digits <- ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(
        x = trends_2$bfd$digits,
        y = trends_2$bfd$benford.dist.freq
      ),
      color = "blue") +
      ggplot2::geom_line(ggplot2::aes(x = trends_2$bfd$digits,
                    y = trends_2$bfd$data.dist.freq),
                color = palette_colors$graylagoon$tangerine) +
      ggplot2::scale_x_continuous(breaks = seq(10, 100, 20)) +
      ggplot2::labs(x = "Digit", y = "Frequency") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.margin = grid::unit(c(5, 30, 5, 5), "pt"))

    plot_result <- single_digit + two_digits

    png_file <- tempfile(fileext = ".png")
    ggplot2::ggsave(
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
