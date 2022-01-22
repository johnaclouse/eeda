# Create color palettes ----
color_pal <- c(
  "black",
  "red",
  "darkgreen",
  "darkmagenta",
  "darkorange4",
  "hotpink",
  "navyblue",
  "#FF7F00", # redorange
  "gray50",
  "darkred",
  "darkslateblue",
  "#4DAF4A", # green
  "darkslategray",
  "#b8a787", # wheat
  "cyan",
  "#FFFF33", # yellow
  "deepskyblue",
  "seagreen1",
  "darksalmon",
  "darkorchid4",
  "green1",  # light green
  "indianred3",
  "#377EB8", # blue
  "orange",
  "darkorchid1")

# display vector of colors
# pie(rep(1, length(color_pal)), col = color_pal)

palette_colors <- list()
palette_colors <- purrr::map(seq_along(color_pal),
                      ~color_pal[1:.])

# display list of colors
# walk(seq_along(color_pal),
# ~ pie(rep(1, .), col = palette_colors[[.]]))

# over rides and special color combinations
palette_colors[["BlBr"]] <- color_pal[c(23, 4)]
# pie(rep(1, 2), col = palette_colors[["BlBr"]])

palette_colors[["graylagoon"]][["lagoon"]] <- "#005D83"
palette_colors[["graylagoon"]][["bondi"]] <- "#0091B3"
palette_colors[["graylagoon"]][["plum"]] <- "#A50064"
palette_colors[["graylagoon"]][["tangerine"]] <- "#FFCD00"
palette_colors[["graylagoon"]][["gray"]] <- "#767171"

# pie(rep(1,5), col = unlist(palette_colors[["graylagoon"]]))
rm(color_pal)
