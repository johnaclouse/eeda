add_eeda_style <- function ()
{
  if (is.null(getOption("eeda_css_added")) == TRUE) {
    options("eeda_css_added" = TRUE)
    cat(readLines(system.file("extdata", "eeda.css", package = "eeda")), sep="\n")
  }
}


