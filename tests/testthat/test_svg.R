library(two.laws.html)

SetTestContext("svg")

CreateTestCase(
  "Empty svg tags",
  {
    Html <- str_RemoveWhitespace(
      as.character(withTags(svg(g())))
    )

    expect_equal(
      Html,
      "<svg><g></g></svg>"
    )
  }
)
