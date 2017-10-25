#' @include HtmlToTagList.R
#' @include MarkdownToHtml.R
#' @include CodeToHtml.R
NULL

#' @export
HtmlGeneratorBaseClass <- R6Class(
  "HtmlGeneratorBaseClass",
  inherit = BaseClass,
  public = list(


    CodeToHtml = CodeToHtml,
    FunctionToHtml = FunctionToHtml,
    ExcelMarkdownToTagList = ExcelMarkdownToTagList,


    initialize = function(){
      opts_knit$set(
        progress = FALSE,
        verbose = FALSE
      );


      opts_chunk$set(

        # In most cases, we do NOT want to see our code
        echo = FALSE,

        # To avoid surprises, especially when we source() other scripts,
        # lets turn caching off
        cache = FALSE,

        # In most cases, we do not want to see any warnings or messages in
        # our output.  But, we are still responsible for unit testing each
        # chunk.
        warning = FALSE,
        message = FALSE,

        # We do not want to overlook errors, therefore we we want to stop on errors.
        # And yes, this setting seems backwards, check the docs.
        error = FALSE,

        # If we added white space, let's keep it
        strip.white = FALSE
      )

    }
  )
)


#' @export
BB.HtmlGeneratorBaseClass <- function(){
  HtmlGeneratorBaseClass$new()
}