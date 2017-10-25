#' @include HtmlToTagList.R
NULL

#' @export
MarkdownToHtml <- function(rawMarkdown, escapeEscapes = FALSE){


  Html <- markdown::markdownToHTML(text = rawMarkdown, fragment.only = TRUE)

  # for some reason, ~ does not subscript on windows like it does on mac
  if (str_detect(Html, "~([^~<>])~")){
    Html <- str_replace_all(Html, "~([^~<>])~", "<sub>\\1</sub>")
  }

  if(escapeEscapes){
    Html %<>%
      str_replace_all(fixed("&"), "@@@") %>%
      str_replace_all(fixed("#"), "!!!") %>%
      str_replace_all(fixed("\\"), "\\\\")
  }

  return(Html)

}

#' @export
MarkdownToHtmlTagList <- function(rawMarkdown){


  Html <- MarkdownToHtml(rawMarkdown, escapeEscapes = TRUE)


  HtmlLines <- unlist(strsplit(Html, "\n"))
  assert_all_are_not_matching_fixed(HtmlLines, "&")
  assert_all_are_not_matching_fixed(HtmlLines, "#")


  TagList <- HtmlToTagList(Html)

  return(TagList)
}



#' @export
UnescapeEntities <- function(x){

  x %<>%
    str_replace_all(fixed("!!!"),  "#") %>%
    str_replace_all(fixed("@@@"),  "&") %>%
    str_replace_all(fixed("&#39;"), "'") %>%
    str_replace_all(fixed("&#34;"), '"') %>%
    str_replace_all(fixed("&lt;"), '<') %>%
    str_replace_all(fixed("&gt;"), '>') %>%
    str_replace_all(fixed("&rdquo;"), '"') %>%
    str_replace_all(fixed("&ldquo;"), '"') %>%
    str_replace_all(fixed("&mdash;"), '-') %>%
    str_replace_all(fixed("&ndash;"), '-')


  return(x)
}

#' @export
UnescapeTagList <- function(tagList){

  TagList <- rapply(
    tagList,
    UnescapeEntities,
    classes = "character",
    how = "replace"
  )

  return(TagList)

}

#' @export
TagListToHtml <- function(tagList){

  #Html <- as.character(tagList)
  HtmlEscaped <- knitr::asis_output(tagList)

  Html <- rapply(
    HtmlEscaped,
    function(x) {
      x %>%
        str_replace_all(fixed("!!!"),  "#") %>%
        str_replace_all(fixed("@@@"),  "&")
    },
    classes = "character",
    how = "replace"
  )

  unescapedtaglist <- rapply(
    tagList,
    function(x) {
      x %>%
        str_replace_all(fixed("!!!"),  "#") %>%
        str_replace_all(fixed("@@@"),  "&")
    },
    classes = "character",
    how = "replace"
  )


  return(Html)

}


#' @export
ExcelMarkdownToTagList <- function(excelMarkdown){

  # Make sure unnecessary linebreaks still have a space between words
  Markdown <- str_replace_all(excelMarkdown, fixed("\n"), " \n")

  MarkdownToHtmlTagList(Markdown)

}

