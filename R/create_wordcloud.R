create_wordcloud <-
  function(x,
           word_count = 125,
           width = 1294 / 72,
           height = 800 / 72,
           dpi = 72) {
    
    
    # word cloud gets stuck when there are only unique values
    if (sum(duplicated(x)) > 1) {
      
      top_words <- as_tibble(table(x)) %>% 
        filter(x != "") %>% 
        mutate(x = stringr::str_trunc(x, 30)) %>% 
        arrange(desc(n)) %>% 
        slice(1:word_count)
      
      word_cloud <-
        ggplot(top_words) +
        ggwordcloud::geom_text_wordcloud(
          aes(
            label = x,
            size = n,
            color = factor(n)
          ),
          rm_outside = TRUE,
          max_steps = 1,
          grid_size = 1,
          eccentricity = 0.9
        ) +
        # scale_radius(range = c(2, 18), limits = c(0, NA)) +
        scale_size_area(max_size = 14) +
        scale_color_viridis_d(option = "H", direction = -1) +
        theme_minimal() 
      
      png_file <- tempfile(fileext = ".png")
      ggsave(
        filename = png_file,
        dev = "png",
        plot = word_cloud,
        width = width,
        height = height ,
        dpi = dpi
      )
      # crop ggplot2 image saved as png
      knitr::plot_crop(png_file) 
    } else {
      png_file <- ""
    }
    return(png_file)
  }


