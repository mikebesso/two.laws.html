#' @include polymorphism.R

#' @export
HighlightCode <- function(code){

  # Covert the string of code to a file of lines
  Code <- str_split(code, "\n", simplify = TRUE)[1, ]

  TempInputFile <- tempfile()
  TempOutputFile <- tempfile()

  writeLines(Code, con = TempInputFile)

  # Highlight the file
  highlight::highlight(
    file = TempInputFile,
    renderer = highlight::renderer_html(document = FALSE),
    final.newline = TRUE,
    show_line_numbers = TRUE,
    output = TempOutputFile
  )

  # Get the highlighted file
  Lines <- readLines(TempOutputFile)

  # Return the highlighted file
  return(Lines)
}


#' @export
CodeToHtml <- function(code, divClass = 'code-r') {

  # Add syntax highlighting
  Lines <- HighlightCode(code)

  # Wrap the code (read from the temp file) in a div with a class
  Html <- as.html(
    c(
      paste0("<div class='", divClass, "'>"),
      Lines,
      "</div>"
    )
  )

  # Return the Html
  return(Html)

}


#' @export
FunctionToHtml <- function(f, divClass = "code-r", fName = substitute(f)){

  # Try to get the actual source code
  Code <- GetFunctionDefinition(f, fName)

  # Turn the code into Html
  Html <- CodeToHtml(Code, divClass)

  # Return the Html
  return(Html)


}



