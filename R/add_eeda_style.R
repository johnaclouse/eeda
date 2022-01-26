add_extra_eda_style <- function ()
{
  if (is.null(getOption("extra_eda_css_added")) == TRUE) {
    options("extra_eda_css_added" = TRUE)
    cat(readLines(system.file("extdata", "extra_eda.css", package = "extra_eda")), sep="\n")
  }
}


