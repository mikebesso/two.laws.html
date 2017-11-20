library(testthat)

context("MarkdownToHtml Headings")

test_that(
  "Headings",
  {
    for (i in 1:6){
      Heading <- "Héading"

      Markdown <- paste(paste0(rep("#", i), collapse = ""), Heading)
      Html <- paste0("<h", i, ">", Heading, "</h", i, ">\n")

      expect_equal(
        MarkdownToHtml(Markdown),
        Html
      )
    }
  }
)


test_that(
  "Special Characters",
  {
    SpecialCharacters <- c(
      `&` = "&amp;",
      `<` = "&lt;",
      `>` = "&gt;",
      `'` = "&#39;",
      `"` = "&ldquo;",
      `""` = "&ldquo;&rdquo;"
    )

    Markdown <- (paste("stuff before", names(SpecialCharacters)))

    # Entities are getting a space before them when text is before them
    # need to check if that is OK with quotes.
    Html <- paste0("<p>stuff before ", SpecialCharacters, "</p>\n")

    for (i in 1:length(Markdown)){
      expect_equal(
        MarkdownToHtml(Markdown[[i]]),
        Html[[i]]
      )
    }
  }
)

test_that(
  "Quoting Text",
  {
    SpecialCharacters <- c(
      `'quote'` = "&#39;quote&#39;",
      `"quote"` = "&ldquo;quote&rdquo;"
    )

    Markdown <- paste("stuff before", names(SpecialCharacters))

    # Entities are getting a space before them when text is before them
    # need to check if that is OK with quotes.
    Html <- paste0("<p>stuff before ", SpecialCharacters, "</p>\n")

    for (i in 1:length(Markdown)){
      expect_equal(
        MarkdownToHtml(Markdown[[i]]),
        Html[[i]]
      )
    }
  }
)