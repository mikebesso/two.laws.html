

HtmlToTagList<-function(rawHtml){

  # TODO:  Before we get started, we also need to
  # Remove all comments  <!--   --!>
  # Handle html entities


  TOKENS <- data_frame(
    raw = c('="', '">', '"'), #, "'"),   # , "(", ")"),
    escaped = c("=&QUOT", "&QUOT>", "&quot;") #, "&#039;", "&#40", "&#41")
  )



  EscapeHtmlToTaglist <- function(string) {
    for (i in seq_along(TOKENS$raw)) {
      raw <- TOKENS$raw[i]
      escaped <- TOKENS$escaped[i]
      string <- gsub(raw, escaped, string, fixed = TRUE)
    }
    string
  }


  UnescapeHtmlToTaglist <- function(string) {
    for (i in rev(seq_along(TOKENS$raw))) {
      raw <- TOKENS$raw[i]
      escaped <- TOKENS$escaped[i]
      string <- gsub(escaped, raw, string, fixed = TRUE)
    }
    string
  }

  #escapedHtml <- EscapeHtmlToTaglist(rawHtml)

  # Get rid of line feeds
  #escapedHtml <- str_replace_all(escapedHtml, fixed("\n"), "")

  escapedHtml <- str_replace_all(rawHtml, fixed("\n"), "")

  # replace empty self closing tags with full tags
  escapedHtml <- str_replace_all(escapedHtml, "<(\\w+)\\s*/>", "<\\1></\\1>")



  # Split our input into tokens based on tags

  SplitHtml <- escapedHtml  %>%
    str_replace_all("<\\/[^>]*>", "~~~@CLOSE~~~") %>%
    str_replace_all("<([^ //>])", "~~~@OPEN_LEFT_\\1") %>%
    str_replace_all(">", "~~~@OPEN_RIGHT~~~") %>%
    str_split("~~~")
  TagCodeList <- SplitHtml[[1]]

  # Get rid of any empty items in our split list
  TagCodeList <- TagCodeList[TagCodeList != ""]



  #replace " with '
  TagCodeList <- gsub('"', "'", TagCodeList)


  # #replace close tag with ),
  # TagCodeList <- gsub('</(.*?)>', "),", TagCodeList)

  # We are not yet handling:
  #    * ampersands in href arguments


  # Find tags with attributes
  attrIdx <- intersect(
    grep('=', TagCodeList),
    which(stri_startswith_fixed(TagCodeList, '@OPEN_LEFT_'))
  )

  if (length(attrIdx) > 0){
    attrVal <- TagCodeList[attrIdx]
    attrVal <- gsub('@OPEN_RIGHT', ",", attrVal)
    attrVal <- gsub('@OPEN_LEFT_', 'tags$', attrVal)
    attrVal <- sub('^ ', '', attrVal)
    attrVal <- sub(' ', "(", attrVal)

    if(length(TagCodeList[attrIdx]) != length(attrVal)) browser()

    # Add commas between attributes
    attrVal <- str_replace_all(
      attrVal,
      "\\s(['a-zA-Z_$0-9'-]*)=",
      ', \\1='
    )


    TagCodeList[attrIdx] <- attrVal
    #TagCodeList[attrIdx] <- str_replace_all(TagCodeList[attrIdx], "&QUOT", "'")
  }


  # Open the tags$??() function call
  TagCodeList <- str_replace(TagCodeList, "@OPEN_LEFT_(.*)", "tags$\\1(")

  # Add commas between arguments to tags$??()
  TagCodeList <- str_replace(TagCodeList, "@OPEN_RIGHT", ",")

  # Close the tags$??() function call
  TagCodeList <- str_replace(TagCodeList, "@CLOSE", "), ")

  # Get rid of any extra commas
  CommaIndices <-  which(TagCodeList == "), ")
  CommasToRemove <- intersect(CommaIndices, CommaIndices - 1)
  if (length(CommasToRemove) > 0){
    TagCodeList[CommasToRemove] <- ")"
  }

  # Get rid of initial commas
  TagsNoAttributesIndices <- which(str_detect(TagCodeList, "^tags\\$.*\\($"))
  OnlyCommasIndices <- which(TagCodeList == ",")
  InitialCommasToRemove <- intersect(TagsNoAttributesIndices+ 1, OnlyCommasIndices)
  if (length(InitialCommasToRemove) > 0){
    TagCodeList <- TagCodeList[-InitialCommasToRemove]
  }



  # Add quotes to inside html text
  InsideHtmlIndices <- c(
    which(!str_detect(TagCodeList, "^tags\\$") & !(TagCodeList %in% c(")", "), ", ","))),
    which(TagCodeList == "," & data.table::shift(TagCodeList == "), ", n = 1, fill = FALSE))
  )
  if (length(InsideHtmlIndices) > 0){
    TagCodeList[InsideHtmlIndices] <- paste0('"', TagCodeList[InsideHtmlIndices], '"')
  }



  # Add commas between tags
  TagsIndices <- which(str_detect(TagCodeList, "^tags\\$"))
  TagsInsideTags <- intersect(InsideHtmlIndices + 1, TagsIndices)
  if (length(TagsInsideTags) > 0){
    TagCodeList[TagsInsideTags] <- paste0(", ", TagCodeList[TagsInsideTags])
  }


  # Remove last comma
  TagCodeList[length(TagCodeList)] <- ")"


  # Collaps our tokens into a single string
  TagCode <- paste0(TagCodeList, collapse = '')


  #TagCodeList <- UnescapeHtmlToTaglist(TagCodeList)


  #eval to tagList object
  TagList <- eval(parse(text=sprintf('tagList(list(%s))', TagCode)))

  return(TagList)
}


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

MarkdownToHtmlTagList <- function(rawMarkdown){


  Html <- MarkdownToHtml(rawMarkdown, escapeEscapes = TRUE)


  HtmlLines <- unlist(strsplit(Html, "\n"))
  assert_all_are_not_matching_fixed(HtmlLines, "&")
  assert_all_are_not_matching_fixed(HtmlLines, "#")


  TagList <- HtmlToTagList(Html)

  return(TagList)
}


ExcelMarkdownToTagList <- function(excelMarkdown){

  # Make sure unnecessary linebreaks still have a space between words
  Markdown <- str_replace_all(excelMarkdown, fixed("\n"), " \n")

  MarkdownToHtmlTagList(Markdown)

}

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

UnescapeTagList <- function(tagList){

  TagList <- rapply(
    tagList,
    UnescapeEntities,
    classes = "character",
    how = "replace"
  )

  return(TagList)

}

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

ExcelMarkdownToTagList <- function(excelMarkdown){

  # Make sure unnecessary linebreaks still have a space between words
  Markdown <- str_replace_all(excelMarkdown, fixed("\n"), " \n")

  MarkdownToHtmlTagList(Markdown)

}
