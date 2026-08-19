# library(Characterization)
# library(testthat)

context("generate")

test_that("generateProtocol", {
  
  test <- generateProtocol(
    jsonLocation = getDemoLoc(),
    webAPI = 'https://api.ohdsi.org/WebAPI',
    outputLocation = './protocol', 
    downloadConcepts = FALSE,
    headerLogoLocation = file.path(R.home('doc'), 'html', 'logo.jpg')
  )
  
  testthat::expect_true(dir.exists('./protocol'))
  testthat::expect_true(file.exists(test))
  protocolHtml <- paste(readLines(test, warn = FALSE), collapse = "\n")
  testthat::expect_match(protocolHtml, "background-color: #336B91 !important;", fixed = TRUE)
  testthat::expect_match(protocolHtml, ".quarto-title-block .quarto-title-meta { display: none !important; }", fixed = TRUE)
  testthat::expect_match(protocolHtml, "protocol-title-date", fixed = TRUE)
  testthat::expect_match(protocolHtml, "body.quarto-light { margin-left: 0; }", fixed = TRUE)
  testthat::expect_match(protocolHtml, "#quarto-content.toc-left { margin-left: 0; padding-left: 0; }", fixed = TRUE)
  testthat::expect_match(protocolHtml, "#quarto-sidebar-toc-left { grid-column: screen-start / body-start; background-color: #f4f5f6; border-right: 1px solid #d9dee3; margin-left: 0; padding-left: 0; }", fixed = TRUE)
  testthat::expect_match(protocolHtml, "#quarto-sidebar-toc-left #TOC { margin-left: 0; padding: 1rem 1.1rem; }", fixed = TRUE)
  testthat::expect_match(protocolHtml, "data:image/jpeg;base64,", fixed = TRUE)
  testthat::expect_false(grepl("Human readable study specification", protocolHtml, fixed = TRUE))

  testWithSubheading <- generateProtocol(
    jsonLocation = getDemoLoc(),
    webAPI = 'https://api.ohdsi.org/WebAPI',
    outputLocation = './protocol',
    outputName = basename(tempfile(pattern = 'protocol_subheading_test_', fileext = '.html')),
    downloadConcepts = FALSE,
    protocolSubheading = "Human readable study specification"
  )

  protocolHtmlWithSubheading <- paste(readLines(testWithSubheading, warn = FALSE), collapse = "\n")
  testthat::expect_match(protocolHtmlWithSubheading, "Human readable study specification", fixed = TRUE)
  testthat::expect_match(protocolHtmlWithSubheading, "protocol-title-subheading", fixed = TRUE)
  
  
})

