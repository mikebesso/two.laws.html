
#' @export
as.html <- function(x){
  Html <- paste(x, collapse = "\n")
  class(Html) <- c("html", "chr")
  return(Html)
}