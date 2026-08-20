context("cohort method")

make_cohort_definition_df <- function() {
  data.frame(
    cohortId = c(10, 11, 20, 21, 30, 31, 40, 41, 99),
    cohortName = c(
      "Target A", "Target B",
      "Comp A", "Comp B",
      "Outcome A", "Outcome B",
      "Neg A", "Neg B",
      "Nesting"
    ),
    cohortNameWithLink = c(
      "Target A <a href='#cohort-10'>View</a>",
      "Target B <a href='#cohort-11'>View</a>",
      "Comp A <a href='#cohort-20'>View</a>",
      "Comp B <a href='#cohort-21'>View</a>",
      "Outcome A <a href='#cohort-30'>View</a>",
      "Outcome B <a href='#cohort-31'>View</a>",
      "Neg A <a href='#cohort-40'>View</a>",
      "Neg B <a href='#cohort-41'>View</a>",
      "Nesting <a href='#cohort-99'>View</a>"
    ),
    parentId = c(10, 11, 20, 21, 30, 31, 40, 41, 99),
    parentName = c("Target A", "Target B", "Comp A", "Comp B", "Outcome A", "Outcome B", "Neg A", "Neg B", "Nesting"),
    subsetId = c(-1, -1, -1, -1, -1, -1, -1, -1, -1),
    isParent = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    subsetName = rep(NA_character_, 9),
    packageVersion = rep(NA_character_, 9),
    numberSubsetOperators = rep(NA_integer_, 9),
    subsetCohorts = rep("", 9),
    appliedSubsets = rep("", 9),
    stringsAsFactors = FALSE
  )
}

make_settings_cm <- function() {
  list(
    targetComparatorOutcomesList = list(
      list(
        targetId = 10,
        comparatorId = 20,
        nestingCohortId = 99,
        excludedCovariateConceptIds = c(100, 101),
        outcomes = list(
          list(outcomeId = 30, outcomeOfInterest = TRUE, priorOutcomeLookback = 365),
          list(outcomeId = 31, outcomeOfInterest = TRUE, priorOutcomeLookback = 30),
          list(outcomeId = 40, outcomeOfInterest = FALSE, priorOutcomeLookback = 0)
        )
      ),
      list(
        targetId = 11,
        comparatorId = 21,
        excludedCovariateConceptIds = c(100, 102),
        outcomes = list(
          list(outcomeId = 30, outcomeOfInterest = TRUE, priorOutcomeLookback = 60),
          list(outcomeId = 41, outcomeOfInterest = FALSE)
        )
      )
    ),
    cmAnalysisList = list(
      list(
        analysisId = 1,
        description = "analysis one",
        createStudyPopulationArgs = list(
          startAnchor = "cohort start",
          riskWindowStart = 1,
          endAnchor = "cohort end",
          riskWindowEnd = 30
        )
      ),
      list(
        analysisId = 2,
        description = "analysis two",
        createStudyPopulationArgs = list(
          startAnchor = "cohort start",
          riskWindowStart = 1,
          endAnchor = "cohort end",
          riskWindowEnd = 30
        )
      )
    ),
    cmDiagnosticThresholds = list(maxStdDiff = 0.1),
    refitPsForEveryOutcome = TRUE,
    refitPsForEveryStudyPopulation = FALSE
  )
}


test_that("extractCohortMethodSettings works when negativeControls is NULL", {
  cohort_definition <- make_cohort_definition_df()
  settings_cm <- make_settings_cm()

  module_settings <- list(settings = settings_cm)

  result <- ProtocolGenerator:::extractCohortMethodSettings(
    cohortMethodModuleSettings = module_settings,
    negativeControls = NULL,
    cohortDefinitionDf = cohort_definition
  )

  testthat::expect_is(result, "list")
  testthat::expect_true(all(c(
    "negativeControlsCM", "tcCombos", "outcomeRange", "compIds",
    "targetParentsCount", "targetCohortCount", "tars", "analysisCm",
    "cmOutUnique", "commonExclude", "nonCommonSets", "commonNegativeId",
    "nonCommonNegSets", "diagSetting", "refitPsForEveryOutcome",
    "refitPsForEveryStudyPopulation"
  ) %in% names(result)))

  testthat::expect_is(result$negativeControlsCM, "data.frame")
  testthat::expect_true(all(c("cohortId", "outcomeConceptId", "cohortName") %in% colnames(result$negativeControlsCM)))
  testthat::expect_true(setequal(result$negativeControlsCM$cohortId, c(40, 41)))

  testthat::expect_equal(result$targetParentsCount, 2)
  testthat::expect_equal(result$targetCohortCount, 2)
  testthat::expect_equal(result$outcomeRange, " between 1 and 2")
  testthat::expect_equal(length(result$tars), 1)

  testthat::expect_true(all(c("cohortNameTarget", "cohortNameComp", "sameSubset", "outcomeSet") %in% colnames(result$tcCombos)))
  testthat::expect_true(any(grepl("sec-cm-out-1", result$tcCombos$outcomeSet, fixed = TRUE)))
  testthat::expect_true(any(grepl("sec-cm-out-2", result$tcCombos$outcomeSet, fixed = TRUE)))
  testthat::expect_true(any(grepl("sec-cm-exclude-", result$tcCombos$additionalExclusions, fixed = TRUE)))
  testthat::expect_true(any(grepl("sec-cm-negset-", result$tcCombos$additionalNegativeControlId, fixed = TRUE)))

  testthat::expect_equal(result$commonExclude, 100)
  testthat::expect_equal(length(result$nonCommonSets), 2)
  testthat::expect_true(length(result$commonNegativeId) == 0)
  testthat::expect_equal(length(result$nonCommonNegSets), 2)

  testthat::expect_is(result$diagSetting, "list")
  testthat::expect_equal(result$diagSetting$maxStdDiff, 0.1)
  testthat::expect_equal(result$refitPsForEveryOutcome, TRUE)
  testthat::expect_equal(result$refitPsForEveryStudyPopulation, FALSE)
})


test_that("extractCohortMethodSettings uses cmAnalysesSpecifications and provided negative controls", {
  cohort_definition <- make_cohort_definition_df()
  settings_cm <- make_settings_cm()

  provided_negative_controls <- data.frame(
    cohortId = 999,
    outcomeConceptId = 999,
    outcomeOfInterest = FALSE,
    priorOutcomeLookback = 0,
    cohortName = "Provided NC",
    stringsAsFactors = FALSE
  )

  module_settings <- list(
    settings = list(
      cmAnalysesSpecifications = settings_cm,
      refitPsForEveryOutcome = FALSE,
      refitPsForEveryStudyPopulation = TRUE
    )
  )

  result <- ProtocolGenerator:::extractCohortMethodSettings(
    cohortMethodModuleSettings = module_settings,
    negativeControls = provided_negative_controls,
    cohortDefinitionDf = cohort_definition
  )

  testthat::expect_equal(result$negativeControlsCM, provided_negative_controls)
  testthat::expect_equal(result$refitPsForEveryOutcome, TRUE)
  testthat::expect_equal(result$refitPsForEveryStudyPopulation, FALSE)
  testthat::expect_equal(length(result$analysisCm), 2)
})


test_that("cmColDef returns full and filtered column definitions", {
  testthat::skip_if_not_installed("reactable")

  all_defs <- ProtocolGenerator:::cmColDef()

  testthat::expect_is(all_defs, "list")
  testthat::expect_true(all(c("parentNameTarget", "sameSubset", "outcomeSet") %in% names(all_defs)))
  testthat::expect_s3_class(all_defs$parentNameTarget, "colDef")
  testthat::expect_s3_class(all_defs$sameSubset, "colDef")

  filtered_defs <- ProtocolGenerator:::cmColDef(
    elementId = "custom-table-id",
    colNames = c("parentNameTarget", "sameSubset")
  )

  testthat::expect_equal(names(filtered_defs), c("parentNameTarget", "sameSubset"))
  testthat::expect_s3_class(filtered_defs$parentNameTarget, "colDef")
  testthat::expect_s3_class(filtered_defs$sameSubset, "colDef")
})
