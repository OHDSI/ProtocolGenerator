

context("helpers")

jsonFileLoc <- file.path('../resources', 'example.json')
json <- ParallelLogger::loadSettingsFromJson(jsonFileLoc)

test_that("getCohortDefinitionsFromJson", {
  
  cohortDef <- getCohortDefinitionsFromJson(json)
  testthat::expect_true("cohortDefinitions" %in% names(cohortDef))
  
  testthat::expect_equal(
    length(cohortDef$cohortNames), 
    length(cohortDef$cohortDefinitions)
  )
  
})

test_that("getCohortDefinitionsFromJson", {
  
res <- getConcepts(
  conceptIds = 21600712, 
  baseUrl = 'https://api.ohdsi.org/WebAPI'
  )

testthat::expect_is(res, 'list')

})



test_that("getFunctionFromArgName", {
  
  fun <- getFunctionFromArgName(
    package = 'CohortMethod', 
    argumentName = 'createPsArgs'
  )
  
  testthat::expect_equal(fun, 'createPs')
  
})


test_that("getHelpText", {
  
  help <- getHelpText(
    package = 'CohortMethod', 
    functionName = 'createPs',
    input = 'maxCohortSizeForFitting'
  )
  
  testthat::expect_is(help, 'character')
  
})


test_that("getAllHelpText", {
  
  helpAll <- getAllHelpText(
    package = 'CohortMethod', 
    functionName = 'createPs'
  )
  
  testthat::expect_is(helpAll, 'data.frame')
  
})

test_that("getExtraCyclopsHelp", {
  
  helpC <- getExtraCyclopsHelp()
  
  testthat::expect_is(helpC$control, 'data.frame')
  testthat::expect_is(helpC$prior, 'data.frame')
})




test_that("functionDefaults", {
  
  defaults <- functionDefaults(
    package = 'CohortMethod', 
    functionName = 'createPs'
  )
  
  testthat::expect_is(defaults, 'list')
  testthat::expect_true(length(defaults) > 0 )
})

test_that("getAllHelpDetails", {
  
  allHelpDetails <- getAllHelpDetails(
    package = 'CohortMethod', 
    functionName = 'createPs'
  )
  
  testthat::expect_is( allHelpDetails, 'data.frame')
  testthat::expect_true(ncol( allHelpDetails) > 0 )
})


test_that("getSettingsTable and defaultColumns", {
  
  allSettings <- getSettingsTable(
    package = 'CohortMethod', 
    functionName = 'createPs',
    settings = json$moduleSpecifications[[5]]$settings$cmAnalysisList[[1]]$createPsArgs
  )
  
  testthat::expect_is( allSettings, 'data.frame')
  testthat::expect_true(ncol( allSettings) > 0 )
  
  
  tbl <- defaultColumns(allSettings)
  testthat::expect_is(tbl, 'list')
  testthat::expect_true(sum(names(tbl) %in% c("value",  "level2", "bold")) == 3 )
  
  
  tblReact <- reportTableFormat(
    table = allSettings, 
    groupBy = NULL,
    columns = tbl, 
    caption
  )
  
  testthat::expect_is(tblReact, "reactable")
  
})


test_that("formatCovariateSettings", {
  
  
  cov1 <- formatCovariateSettings(
    covariateSettings = json$moduleSpecifications[[3]]$settings$aggregateCovariateSettings[[1]]$covariateSettings
  )
  testthat::expect_is(cov1, 'data.frame')
  
  cov2 <- formatCovariateSettings(
    covariateSettings = json$moduleSpecifications[[5]]$settings$cmAnalysisList[[1]]$getDbCohortMethodDataArgs$covariateSettings
  )
  testthat::expect_is(cov2, 'data.frame')
  
  for(j in 1:length(json$moduleSpecifications[[7]]$settings)){
    cov3 <- formatCovariateSettings(
      covariateSettings = json$moduleSpecifications[[7]]$settings[[j]]$covariateSettings
    )
    testthat::expect_is(cov3, 'data.frame')
  }

})

#listToDf
