# library(Characterization)
# library(testthat)

context("generate")

test_that("generateProtocol", {
  
  test <- generateProtocol(
    jsonLocation = getDemoLoc(),
    webAPI = 'https://api.ohdsi.org/WebAPI',
    outputLocation = './protocol', 
    downloadConcepts = FALSE
  )
  
  testthat::expect_true(dir.exists('./protocol'))
  # TODO check html file generates:
  
  
})

