context("patient level prediction")

make_plp_cohort_definition_df <- function() {
  data.frame(
    cohortId = c(101, 102, 201, 202),
    cohortName = c("Target A", "Target B", "Outcome A", "Outcome B"),
    cohortNameWithLink = c(
      "Target A <a href='#cohort-101'>View</a>",
      "Target B <a href='#cohort-102'>View</a>",
      "Outcome A <a href='#cohort-201'>View</a>",
      "Outcome B <a href='#cohort-202'>View</a>"
    ),
    parentId = c(1001, 1002, 2001, 2002),
    parentName = c("Parent Target A", "Parent Target B", "Parent Outcome A", "Parent Outcome B"),
    subsetId = c(-1, -1, -1, -1),
    isParent = c(TRUE, TRUE, TRUE, TRUE),
    subsetName = rep(NA_character_, 4),
    packageVersion = rep(NA_character_, 4),
    numberSubsetOperators = rep(NA_integer_, 4),
    subsetCohorts = rep("", 4),
    appliedSubsets = rep("", 4),
    stringsAsFactors = FALSE
  )
}

make_covariate_settings <- function(use_age = TRUE, use_gender = FALSE) {
  x <- list(
    useDemographicsAge = use_age,
    useDemographicsGender = use_gender,
    nonLogicalSetting = "ignore"
  )
  attr(x, "fun") <- "createCovariateSettings"
  class(x) <- "covariateSettings"
  x
}

make_plp_module_settings <- function() {
  list(
    settings = list(
      modelDesignList = list(
        list(
          targetId = 101,
          outcomeId = 201,
          covariateSettings = make_covariate_settings(TRUE, FALSE),
          populationSettings = list(
            startAnchor = "cohort start",
            riskWindowStart = 1,
            endAnchor = "cohort end",
            riskWindowEnd = 30
          )
        ),
        list(
          targetId = 102,
          outcomeId = 202,
          covariateSettings = make_covariate_settings(TRUE, FALSE),
          populationSettings = list(
            startAnchor = "cohort start",
            riskWindowStart = 1,
            endAnchor = "cohort end",
            riskWindowEnd = 30
          )
        ),
        list(
          targetId = 101,
          outcomeId = 202,
          covariateSettings = make_covariate_settings(TRUE, TRUE),
          populationSettings = list(
            startAnchor = "cohort start",
            riskWindowStart = 0,
            endAnchor = "cohort end",
            riskWindowEnd = 90
          )
        )
      )
    )
  )
}


test_that("getPlpSettings returns expected structure", {
  cohort_definition <- make_plp_cohort_definition_df()
  plp_settings <- make_plp_module_settings()

  result <- getPlpSettings(
    PatientLevelPredictionModuleSettings = plp_settings,
    cohortDefinitionDf = cohort_definition
  )

  testthat::expect_is(result, "list")
  testthat::expect_true(all(c("targetOutcomeSet", "modelDesignUnique", "predictionSummary") %in% names(result)))
  testthat::expect_is(result$targetOutcomeSet, "data.frame")
  testthat::expect_is(result$predictionSummary, "data.frame")
})


test_that("getPlpSettings merges target and outcome details", {
  cohort_definition <- make_plp_cohort_definition_df()
  plp_settings <- make_plp_module_settings()

  result <- getPlpSettings(
    PatientLevelPredictionModuleSettings = plp_settings,
    cohortDefinitionDf = cohort_definition
  )

  tos <- result$targetOutcomeSet

  testthat::expect_equal(nrow(tos), 3)
  testthat::expect_true(all(c(
    "cohortIdTarget", "cohortNameTarget", "parentNameTarget",
    "cohortIdOutcome", "cohortNameOutcome", "parentNameOutcome",
    "designId"
  ) %in% colnames(tos)))
  testthat::expect_equal(length(unique(tos$designId)), 2)
})


test_that("getPlpSettings predictionSummary includes expected content", {
  cohort_definition <- make_plp_cohort_definition_df()
  plp_settings <- make_plp_module_settings()

  result <- getPlpSettings(
    PatientLevelPredictionModuleSettings = plp_settings,
    cohortDefinitionDf = cohort_definition
  )

  summary_df <- result$predictionSummary

  testthat::expect_equal(nrow(summary_df), 2)
  testthat::expect_true(all(c(
    "model_design", "number_targets", "number_targets_with_subsets",
    "number_outcomes", "number_outcomes_with_subsets", "timeAtRisk", "covariates"
  ) %in% colnames(summary_df)))

  testthat::expect_true(any(grepl("sec-model-design-1", summary_df$model_design, fixed = TRUE)))
  testthat::expect_true(any(grepl("cohort start + 1 - cohort end + 30", summary_df$timeAtRisk, fixed = TRUE)))
  testthat::expect_true(any(grepl("cohort start + 0 - cohort end + 90", summary_df$timeAtRisk, fixed = TRUE)))
  testthat::expect_true(any(grepl("createCovariateSettings", summary_df$covariates, fixed = TRUE)))
})


test_that("getPlpColDefs returns expected reactable colDefs", {
  testthat::skip_if_not_installed("reactable")

  result <- getPlpColDefs()

  testthat::expect_is(result, "list")
  testthat::expect_true(all(c(
    "model_design", "number_targets", "number_targets_with_subsets",
    "number_outcomes", "number_outcomes_with_subsets", "timeAtRisk", "covariates"
  ) %in% names(result)))

  testthat::expect_s3_class(result$model_design, "colDef")
  testthat::expect_s3_class(result$timeAtRisk, "colDef")
  testthat::expect_s3_class(result$covariates, "colDef")
})
