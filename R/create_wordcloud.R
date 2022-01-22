create_wordcloud <-
  function(x,
           word_count = 125,
           width = 1294 / 72,
           height = 800 / 72,
           dpi = 72) {

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
          max_steps = 1,
          grid_size = 1,
          eccentricity = 0.9
        ) +
        # ggplot2::scale_radius(range = c(2, 18), limits = c(0, NA)) +
        ggplot2::scale_size_area(max_size = 14) +
        ggplot2::scale_color_viridis_d(option = "H", direction = -1) +
        ggplot2::theme_minimal()

      png_file <- tempfile(fileext = ".png")
      ggplot2::ggsave(
        filename = png_file,
        dev = "png",
        plot = word_cloud,
        width = width,
        height = height ,
        dpi = dpi
      )
      # crop ggplot2::ggplot2 image saved as png
      knitr::plot_crop(png_file)
    } else {
      png_file <- ""
    }
    return(png_file)
  }


