
CodeToHtml <- function(code) {

  Code <- str_split(code, "\n", simplify = TRUE)[1, ]

  TempFile <- tempfile()


  writeLines(Code, con = TempFile)

  highlight::highlight(
    file = TempFile,
    renderer = highlight::renderer_html(document = FALSE),
    final.newline = TRUE,
    show_line_numbers = TRUE
  )

}



FunctionToHtml <- function(f){

  # Try to get the actual source code
  Code <- attr(f, "srcref")

  # If we have source code, then use it so that we get the commnets
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

  Code2Html(Code)

}



