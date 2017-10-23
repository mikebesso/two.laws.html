context("MarkdownToHtml Headings")

test_that(
  "Markdown Heading 1",
  {
    expect_equal(
      MarkdownToHtml("# Heading 1"),
      "<h1>Heading 1</h>\n"
    )
  }
)