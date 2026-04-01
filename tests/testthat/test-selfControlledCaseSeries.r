context("self controlled case series")

make_sccs_cohort_definition_df <- function() {
  data.frame(
    cohortId = c(101, 102, 103, 201, 202, 999),
    cohortName = c("Exposure A", "Exposure B", "Nesting", "Outcome A", "Outcome B", "Other"),
    cohortNameWithLink = c(
      "Exposure A <a href='#cohort-101'>View</a>",
      "Exposure B <a href='#cohort-102'>View</a>",
      "Nesting <a href='#cohort-103'>View</a>",
      "Outcome A <a href='#cohort-201'>View</a>",
      "Outcome B <a href='#cohort-202'>View</a>",
      "Other <a href='#cohort-999'>View</a>"
    ),
    parentId = c(101, 102, 103, 201, 202, 999),
    parentName = c("Parent Exp A", "Parent Exp B", "Parent Nest", "Parent Out A", "Parent Out B", "Parent Other"),
    subsetId = c(-1, -1, -1, -1, -1, -1),
    isParent = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
    subsetName = rep(NA_character_, 6),
    packageVersion = rep(NA_character_, 6),
    numberSubsetOperators = rep(NA_integer_, 6),
    subsetCohorts = rep("", 6),
    appliedSubsets = rep("", 6),
    stringsAsFactors = FALSE
  )
}

make_sccs_settings <- function() {
  list(
    exposuresOutcomeList = list(
      list(
        jsonId = 1,
        outcomeId = 201,
        nestingCohortId = 103,
        exposures = list(
          list(exposureId = 101, exposureIdRef = "exp1", trueEffectSize = NULL),
          list(exposureId = 102, exposureIdRef = "exp2", trueEffectSize = 1)
        )
      ),
      list(
        jsonId = 2,
        outcomeId = 202,
        nestingCohortId = NULL,
        exposures = list(
          list(exposureId = 101, exposureIdRef = "exp1", trueEffectSize = NULL),
          list(exposureId = 103, exposureIdRef = "exp2", trueEffectSize = 1)
        )
      )
    ),
    sccsAnalysisList = list(
      list(analysisId = 1, description = "analysis one"),
      list(analysisId = 2, description = "analysis two")
    ),
    sccsDiagnosticThresholds = list(maxRr = 2.0, minOutcomeCount = 10)
  )
}


test_that("getSccsSettings settings structure is correct", {

  cohort_definition <- make_sccs_cohort_definition_df()
  
  # Add minimal outcome parent columns to support the function
  cohort_definition$cohortIdOutcome <- cohort_definition$cohortId
  cohort_definition$parentNameOutcome <- cohort_definition$parentName
  cohort_definition$cohortNameOutcome <- cohort_definition$cohortName
  
  sccs_settings <- make_sccs_settings()
  module_settings <- list(settings = list(sccsAnalysesSpecifications = sccs_settings))

  testthat::expect_is(module_settings$settings$sccsAnalysesSpecifications, "list")
  testthat::expect_equal(length(module_settings$settings$sccsAnalysesSpecifications$sccsAnalysisList), 2)
  testthat::expect_equal(module_settings$settings$sccsAnalysesSpecifications$sccsDiagnosticThresholds$maxRr, 2.0)
})


test_that("getSccsSettings uses sccsAnalysesSpecifications when present", {

  sccs_settings <- make_sccs_settings()
  cohort_def <- make_sccs_cohort_definition_df()
  
  # When sccsAnalysesSpecifications is present, function uses it
  module_with_specs <- list(settings = list(sccsAnalysesSpecifications = sccs_settings))
  module_without_specs <- list(settings = sccs_settings)
  
  # Verify structure is preserved with specs
  testthat::expect_equal(
    length(module_with_specs$settings$sccsAnalysesSpecifications$sccsAnalysisList),
    2
  )
  
  # Verify structure is preserved without specs
  testthat::expect_equal(
    length(module_without_specs$settings$sccsAnalysisList),
    2
  )
  
  res1 <- getSccsSettings(
    SelfControlledCaseSeriesModuleSettings = module_with_specs,
    cohortDefinitionDf = cohort_def,
    negativeControls = NULL
    )
  
  res2 <- getSccsSettings(
    SelfControlledCaseSeriesModuleSettings = module_with_specs,
    cohortDefinitionDf = cohort_def,
    negativeControls = NULL
  )
  
  testthat::expect_equal(
    res1, res2
  )
  
})


test_that("getSccsSettings extracts basic exposure structure", {

  sccs_settings <- make_sccs_settings()
  module_without_specs <- list(settings = sccs_settings)
  cohort_def <- make_sccs_cohort_definition_df()
  
  res1 <- getSccsSettings(
    SelfControlledCaseSeriesModuleSettings = module_without_specs,
    cohortDefinitionDf = cohort_def,
    negativeControls = NULL
  )
  
  # two exposure outcomes nests with unknown effect
  testthat::expect_true(nrow(res1$eoOfInt) == 2)
  
  # two negative controls
  testthat::expect_true(nrow(res1$negTab) == 2)
  
  # no shared negative controls
  testthat::expect_true(is.null(res1$negTabShared))
  
  testthat::expect_true(!is.null(res1$sccsAnalysisList))
  testthat::expect_true(!is.null(res1$sccsDiagnosticThresholds))
  
})



test_that("getSccsColDefs returns expected reactable colDef list", {
  testthat::skip_if_not_installed("reactable")

  result <- getSccsColDefs()

  testthat::expect_is(result, "list")
  testthat::expect_true(all(c("cohortNameTarget", "cohortNameOutcome", "parentNameTarget") %in% names(result)))
  testthat::expect_s3_class(result$cohortNameTarget, "colDef")
  testthat::expect_s3_class(result$cohortNameOutcome, "colDef")
  testthat::expect_s3_class(result$parentNameTarget, "colDef")

  # Check that certain columns are hidden
  testthat::expect_false(result$outcomeId$show)
  testthat::expect_false(result$exposureId$show)
  testthat::expect_false(result$nestingId$show)
})
