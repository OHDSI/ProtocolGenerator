context("cohort diagnostics")

make_cd_cohort_definition_df <- function() {
  data.frame(
    cohortId = c(101, 102, 201),
    cohortName = c("Target A", "Target B", "Other"),
    cohortNameWithLink = c(
      "Target A <a href='#cohort-101'>View</a>",
      "Target B <a href='#cohort-102'>View</a>",
      "Other <a href='#cohort-201'>View</a>"
    ),
    parentId = c(101, 102, 201),
    parentName = c("Parent A", "Parent B", "Parent C"),
    subsetId = c(-1, -1, -1),
    isParent = c(TRUE, TRUE, TRUE),
    subsetName = rep(NA_character_, 3),
    packageVersion = rep(NA_character_, 3),
    numberSubsetOperators = rep(NA_integer_, 3),
    subsetCohorts = rep("", 3),
    appliedSubsets = rep("", 3),
    stringsAsFactors = FALSE
  )
}

make_cd_settings <- function(cohort_ids = c(101, 102)) {
  list(
    cohortIds = cohort_ids,
    temporalCovariateSettings = list(
      minCharacterizationMean = 0.01,
      includeTemporal = TRUE,
      optionalThreshold = NULL
    ),
    sampleSize = 1000,
    useCache = FALSE,
    outputFolder = "diagnostics",
    optionalSetting = NULL
  )
}


test_that("getCohortDiagnosticTables builds tables and applies cohort filter", {
  cohort_definition <- make_cd_cohort_definition_df()
  cd_settings <- make_cd_settings(cohort_ids = c(101, 102))

  result <- ProtocolGenerator:::getCohortDiagnosticTables(
    CohortDiagnosticsSettings = cd_settings,
    cohortDefinitionDf = cohort_definition
  )

  testthat::expect_is(result, "list")
  testthat::expect_true(all(c("targetTable", "settingsTable", "featureTable") %in% names(result)))

  testthat::expect_equal(nrow(result$targetTable), 2)
  testthat::expect_true(all(result$targetTable$cohortIdTarget %in% c(101, 102)))
  testthat::expect_equal(colnames(result$targetTable)[1], "parentNameTarget")
  testthat::expect_equal(colnames(result$targetTable)[2], "cohortNameTarget")

  testthat::expect_is(result$settingsTable, "data.frame")
  testthat::expect_true(all(c("input", "value") %in% colnames(result$settingsTable)))
  testthat::expect_true("sampleSize" %in% result$settingsTable$input)
  testthat::expect_true(any(result$settingsTable$value == "NULL"))

  testthat::expect_is(result$featureTable, "data.frame")
  testthat::expect_true(all(c("input", "value") %in% colnames(result$featureTable)))
  testthat::expect_true("minCharacterizationMean" %in% result$featureTable$input)
  testthat::expect_true(any(result$featureTable$value == "NULL"))
})


test_that("getCohortDiagnosticTables uses all cohorts when cohortIds is NULL", {
  cohort_definition <- make_cd_cohort_definition_df()
  cd_settings <- make_cd_settings(cohort_ids = NULL)

  result <- ProtocolGenerator:::getCohortDiagnosticTables(
    CohortDiagnosticsSettings = cd_settings,
    cohortDefinitionDf = cohort_definition
  )

  testthat::expect_equal(nrow(result$targetTable), nrow(cohort_definition))
  testthat::expect_true(setequal(result$targetTable$cohortIdTarget, cohort_definition$cohortId))
})


test_that("getCdCols returns expected reactable colDef list", {
  testthat::skip_if_not_installed("reactable")

  result <- ProtocolGenerator:::getCdCols()

  testthat::expect_is(result, "list")
  testthat::expect_true(all(c("parentNameTarget", "cohortNameTarget", "appliedSubsetsTarget") %in% names(result)))
  testthat::expect_s3_class(result$parentNameTarget, "colDef")
  testthat::expect_s3_class(result$cohortNameTarget, "colDef")
  testthat::expect_s3_class(result$appliedSubsetsTarget, "colDef")
})
