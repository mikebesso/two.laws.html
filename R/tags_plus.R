
ParseFunctionText <- function(text){
  parse(text = text)[[1]]
}



#' @export
AddTagsToHtmlTools <- function(newTags){

  # Remove tags that are already defined
  NewTags <- setdiff(newTags, names(tags))

  # If there are no new tags, then return without doing anything
  if(length(NewTags) == 0) return;

  # Name each tag
  names(NewTags) <- NewTags

  # Create the new tag functions
  NewTagFunctions <- sapply(
    NewTags,
    function(newTag){
      FunctionText <- paste0("htmltools::tag('", newTag, "', list(...))")
      eval(bquote(function(...) .(ParseFunctionText(FunctionText))))
    },
    USE.NAMES = TRUE
  )

  # Combine the new tags with the current set, in the global environment
  assign("tags", c(tags, NewTagFunctions), envir = .GlobalEnv)

  # Update withTags to use the new set of tags, in the global environment
  assign(
    "withTags",
    function (code) {eval(substitute(code), envir = as.list(tags), enclos = parent.frame())},
    envir = .GlobalEnv
  )

}

#' @export
AddSvgTagsToHtmlTools <- function(){
  AddTagsToHtmlTools(
    c("svg", "g", "text", "circle", "line", "defs", "path", "style", "ellipse",
      "marker", "polygon", "rect", "symbol", "textPath", "tspan", "use",
      "polyline", "mask", "pattern", "linearGradient", "title",
      "radialGradient", "mask", "filter", "desc", "clipPath",
      "animateColor", "animateMotion", "animateTransform", "cursor", "tref")
  )
}


.onLoad<- function(libname, pkgname){
  message("Adding SVG tags to HtmlTools")
  AddSvgTagsToHtmlTools()
}
