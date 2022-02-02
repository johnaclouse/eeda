create_wordcloud <-
  function(x,
           word_count = 125,
           width = 600,
           height = 450) {

    # binding variable just to keep R CMD Check from seeing NSE as global variables
    n <- NULL

    # word cloud gets stuck when there are only unique values
    if (sum(duplicated(x)) > 1) {
      top_words <- tidyr::as_tibble(table(x)) %>%
        dplyr::filter(x != "") %>%
        dplyr::mutate(x = stringr::str_trunc(x, 30)) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        dplyr::slice(1:word_count)

      word_cloud <-
        ggplot2::ggplot(top_words) +
        ggwordcloud::geom_text_wordcloud(
          ggplot2::aes(
            label = x,
            size = n,
            color = factor(n)
          ),
          rm_outside = TRUE,
          # max_steps = 1,
          # grid_size = 1,
          eccentricity = 0.75
        ) +
        ggplot2::scale_size_area(max_size = 14) +
        ggplot2::scale_color_viridis_d(option = "H", direction = -1) +
        ggplot2::theme_minimal()

      png_file <- tempfile(fileext = ".png")
      # Note: ggsave was creating an error in grid.Call so switched to png
      grDevices::png(
        png_file,
        width = width,
        height = height)
      print(word_cloud)
      grDevices::dev.off()

      # crop ggplot2::ggplot2 image saved as png
      knitr::plot_crop(png_file)
    } else {
      png_file <- ""
    }
    return(png_file)
  }
