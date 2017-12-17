#' @export
CodeToHtml <- function(code) {

  Code <- str_split(code, "\n", simplify = TRUE)[1, ]

  TempInputFile <- tempfile()
  TempOutputFile <- tempfile()


  writeLines(Code, con = TempInputFile)

  highlight::highlight(
    file = TempInputFile,
    renderer = highlight::renderer_html(document = FALSE),
    final.newline = TRUE,
    show_line_numbers = TRUE,
    output = TempOutputFile
  )

  readLines(TempOutputFile)
}


#' @export
FunctionToHtml <- function(f, divClass = "code-r"){

  # Try to get the actual source code
  Code <- attr(f, "srcref")

  # If we have source code,
  #   then use it so that we get the comments
  #   else grab the body and use it instead
  if (!is.null(Code)){
    Code <- paste0("f <- ", paste(as.character(Code, useSource = TRUE), collapse = "\n"))
  } else {
    Code <- paste(
      deparse(
        functionBody(f)
      ),
      collapse = "\n"
    )
  }

  Html <- CodeToHtml(Code)

  Html <- c(paste0("<div class='", divClass, "'>"), Html, "</div>")

  return(Html)


}


FunctionToHtml(FunctionToHtml)
