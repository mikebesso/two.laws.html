#' @include tags_plus.R



if (!devtools::uses_testthat()){
  devtools::use_testthat()
}




.onLoad<- function(libname, pkgname){

  # To keep knitr from glitching, always set a seed
  set.seed(42)

  # Set options
  knitr::opts_knit$set(
    progress = FALSE,
    verbose = FALSE
  )

  # Set default chunk options
  knitr::opts_chunk$set(
    echo = FALSE,
    cahce = FALSE,
    dpi = 72,
    message = FALSE,
    strip.white = TRUE
  )

  # Add svg tags to htmltools
  AddSvgTagsToHtmlTools()
}
