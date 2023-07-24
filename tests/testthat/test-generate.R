# library(Characterization)
# library(testthat)

context("generate")

test_that("generateProtocol", {
  
  test <- generateProtocol(
    webAPI = 'https://api.ohdsi.org/WebAPI',
    outputLocation = './protocol'
  )
  
  testthat::expect_true(dir.exists('./protocol'))
  # TODO check html file generates:
  
  
})

