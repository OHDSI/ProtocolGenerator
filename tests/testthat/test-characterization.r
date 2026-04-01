context("characterization")

test_that("globalCharacterizationSettings returns expected text", {
  settings <- list(
    settings = list(
      minCharacterizationMean = 0.01,
      minCovariateCount = 25,
      mode = "onCreate",
      minSMD = 0.1,
      outputTable = "work_results.char_output"
    )
  )

  result <- ProtocolGenerator:::globalCharacterizationSettings(settings)

  expected <- paste0(
    "Only covariates that occur >= 0.01 fraction of the population and >= 25 people are returned. ",
    "The risk factor analysis used mode onCreate and only returns covariates where the absolute SMD is >= 0.1.  ",
    "All cohorts created by Characterization will be saved into work_results.char_output within the Strategus work schema."
  )

  testthat::expect_equal(result, expected)
})


test_that("processTar formats risk window text", {
  result <- ProtocolGenerator:::processTar(
    riskWindowStart = -30,
    startAnchor = "cohort start",
    riskWindowEnd = 5,
    endAnchor = "cohort end"
  )

  testthat::expect_equal(result, "(cohort start+-30) - (cohort end+5)")
})


test_that("processTargetBaseineSettings handles NULL and builds target table", {
  null_input <- list(settings = list(analysis = list(targetBaselineSettings = NULL)))
  testthat::expect_null(
    ProtocolGenerator:::processTargetBaseineSettings(
      CharacterizationModuleSettings = null_input,
      cohortDefinitionDf = data.frame()
    )
  )

  testthat::skip_if_not_installed("ParallelLogger")

  cohort_definition <- data.frame(
    cohortId = c(1, 2, 3, 4),
    cohortName = c("Target A", "Target B", "Target C", "Target D"),
    parentName = c("Parent A", "Parent B", "Parent C", "Parent D"),
    stringsAsFactors = FALSE
  )

  settings <- list(
    settings = list(
      analysis = list(
        targetBaselineSettings = list(
          list(
            targetIds = c(1, 2),
            limitToFirstInNDays = 30,
            minPriorObservation = 365,
            covariateSettings = list(useDemographicsGender = TRUE)
          ),
          list(
            targetIds = 3,
            limitToFirstInNDays = 60,
            minPriorObservation = 180,
            covariateSettings = list(useDemographicsGender = TRUE)
          ),
          list(
            targetIds = 4,
            limitToFirstInNDays = 0,
            minPriorObservation = 0,
            covariateSettings = list(useDemographicsAge = TRUE)
          )
        )
      )
    )
  )

  result <- ProtocolGenerator:::processTargetBaseineSettings(
    CharacterizationModuleSettings = settings,
    cohortDefinitionDf = cohort_definition
  )

  testthat::expect_is(result, "list")
  testthat::expect_equal(length(result$settingsJson), 2)
  testthat::expect_equal(nrow(result$tableData), 4)
  testthat::expect_true(all(c("cohortNameTarget", "parentNameTarget", "setting") %in% colnames(result$tableData)))
  testthat::expect_true(any(grepl("sec-char-tb-setting-1", result$tableData$setting, fixed = TRUE)))
  testthat::expect_true(any(grepl("sec-char-tb-setting-2", result$tableData$setting, fixed = TRUE)))
})


test_that("processRiskFactorSettings handles NULL and single target-set branch", {
  null_input <- list(settings = list(analysis = list(riskFactorSettings = NULL)))
  testthat::expect_null(
    ProtocolGenerator:::processRiskFactorSettings(
      CharacterizationModuleSettings = null_input,
      cohortDefinitionDf = data.frame()
    )
  )

  testthat::skip_if_not_installed("ParallelLogger")

  cohort_definition <- data.frame(
    cohortId = c(1, 2, 10, 11),
    cohortName = c("Target A", "Target B", "Outcome A", "Outcome B"),
    parentName = c("Parent T1", "Parent T2", "Parent O1", "Parent O2"),
    stringsAsFactors = FALSE
  )

  settings <- list(
    settings = list(
      analysis = list(
        riskFactorSettings = list(
          list(
            targetIds = c(1, 2),
            limitToFirstInNDays = 30,
            minPriorObservation = 365,
            outcomeIds = 10,
            outcomeWashoutDays = 0,
            riskWindowStart = 1,
            startAnchor = "cohort start",
            riskWindowEnd = 30,
            endAnchor = "cohort end",
            covariateSettings = list(useDemographicsGender = TRUE)
          ),
          list(
            targetIds = c(1, 2),
            limitToFirstInNDays = 30,
            minPriorObservation = 365,
            outcomeIds = 11,
            outcomeWashoutDays = 7,
            riskWindowStart = 0,
            startAnchor = "cohort start",
            riskWindowEnd = 15,
            endAnchor = "cohort end",
            covariateSettings = list(useDemographicsAge = TRUE)
          )
        )
      )
    )
  )

  result <- ProtocolGenerator:::processRiskFactorSettings(
    CharacterizationModuleSettings = settings,
    cohortDefinitionDf = cohort_definition
  )

  testthat::expect_is(result, "list")
  testthat::expect_equal(length(result$outcomeDataList), 1)
  testthat::expect_equal(length(result$settingsJson), 2)
  testthat::expect_equal(nrow(result$targetData), 4)
  testthat::expect_true(all(grepl("sec-char-rf-outcome-1", result$targetData$outcomeSet, fixed = TRUE)))
  testthat::expect_true(any(grepl("sec-char-rf-setting-1", result$targetData$setting, fixed = TRUE)))
  testthat::expect_true(any(grepl("sec-char-rf-setting-2", result$targetData$setting, fixed = TRUE)))
})


test_that("processRiskFactorSettings handles multiple target sets", {
  testthat::skip_if_not_installed("ParallelLogger")

  cohort_definition <- data.frame(
    cohortId = c(1, 2, 10, 11),
    cohortName = c("Target A", "Target B", "Outcome A", "Outcome B"),
    parentName = c("Parent T1", "Parent T2", "Parent O1", "Parent O2"),
    stringsAsFactors = FALSE
  )

  settings <- list(
    settings = list(
      analysis = list(
        riskFactorSettings = list(
          list(
            targetIds = 1,
            limitToFirstInNDays = 30,
            minPriorObservation = 365,
            outcomeIds = 10,
            outcomeWashoutDays = 0,
            riskWindowStart = 1,
            startAnchor = "cohort start",
            riskWindowEnd = 30,
            endAnchor = "cohort end",
            covariateSettings = list(useDemographicsGender = TRUE)
          ),
          list(
            targetIds = 2,
            limitToFirstInNDays = 60,
            minPriorObservation = 180,
            outcomeIds = 11,
            outcomeWashoutDays = 7,
            riskWindowStart = 0,
            startAnchor = "cohort start",
            riskWindowEnd = 15,
            endAnchor = "cohort end",
            covariateSettings = list(useDemographicsAge = TRUE)
          )
        )
      )
    )
  )

  result <- ProtocolGenerator:::processRiskFactorSettings(
    CharacterizationModuleSettings = settings,
    cohortDefinitionDf = cohort_definition
  )

  testthat::expect_equal(length(result$outcomeDataList), 2)
  testthat::expect_equal(nrow(result$targetData), 2)
  testthat::expect_true(any(grepl("sec-char-rf-outcome-1", result$targetData$outcomeSet, fixed = TRUE)))
  testthat::expect_true(any(grepl("sec-char-rf-outcome-2", result$targetData$outcomeSet, fixed = TRUE)))
})


test_that("processCaseSeriesSettings handles NULL and multiple target sets", {
  null_input <- list(settings = list(analysis = list(caseSeriesSettings = NULL)))
  testthat::expect_null(
    ProtocolGenerator:::processCaseSeriesSettings(
      CharacterizationModuleSettings = null_input,
      cohortDefinitionDf = data.frame()
    )
  )

  testthat::skip_if_not_installed("ParallelLogger")

  cohort_definition <- data.frame(
    cohortId = c(1, 2, 10, 11),
    cohortName = c("Target A", "Target B", "Outcome A", "Outcome B"),
    parentName = c("Parent T1", "Parent T2", "Parent O1", "Parent O2"),
    stringsAsFactors = FALSE
  )

  settings <- list(
    settings = list(
      analysis = list(
        caseSeriesSettings = list(
          list(
            targetIds = 1,
            limitToFirstInNDays = 30,
            minPriorObservation = 365,
            outcomeIds = 10,
            outcomeWashoutDays = 0,
            riskWindowStart = 1,
            startAnchor = "cohort start",
            riskWindowEnd = 30,
            endAnchor = "cohort end",
            caseCovariateSettings = list(useDemographicsGender = TRUE),
            casePreTargetDuration = 30,
            casePostOutcomeDuration = 30
          ),
          list(
            targetIds = 2,
            limitToFirstInNDays = 60,
            minPriorObservation = 180,
            outcomeIds = 11,
            outcomeWashoutDays = 7,
            riskWindowStart = 0,
            startAnchor = "cohort start",
            riskWindowEnd = 15,
            endAnchor = "cohort end",
            caseCovariateSettings = list(useDemographicsAge = TRUE),
            casePreTargetDuration = 14,
            casePostOutcomeDuration = 14
          )
        )
      )
    )
  )

  result <- ProtocolGenerator:::processCaseSeriesSettings(
    CharacterizationModuleSettings = settings,
    cohortDefinitionDf = cohort_definition
  )

  testthat::expect_is(result, "list")
  testthat::expect_equal(length(result$outcomeDataList), 2)
  testthat::expect_equal(length(result$settingsJson), 2)
  testthat::expect_equal(nrow(result$targetData), 2)
  testthat::expect_true(any(grepl("sec-char-cs-outcome-1", result$targetData$outcomeSet, fixed = TRUE)))
  testthat::expect_true(any(grepl("sec-char-cs-outcome-2", result$targetData$outcomeSet, fixed = TRUE)))
  testthat::expect_true(any(grepl("sec-char-cs-setting-1", result$targetData$setting, fixed = TRUE)))
  testthat::expect_true(any(grepl("sec-char-cs-setting-2", result$targetData$setting, fixed = TRUE)))
})


test_that("characterizationColDef returns expected reactable column definitions", {
  testthat::skip_if_not_installed("reactable")

  result <- ProtocolGenerator:::characterizationColDef()

  testthat::expect_is(result, "list")
  testthat::expect_true(all(c("cohortNameTarget", "cohortNameOutcome", "tar", "setting") %in% names(result)))
  testthat::expect_s3_class(result$tar, "colDef")
  testthat::expect_s3_class(result$setting, "colDef")
})
