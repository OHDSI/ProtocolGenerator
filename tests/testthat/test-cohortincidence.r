context("cohort incidence")

make_ci_cohort_definition_df <- function() {
  data.frame(
    cohortId = c(101, 102, 201, 202, 999),
    cohortName = c("Target A", "Target B", "Outcome A", "Outcome B", "Other"),
    cohortNameWithLink = c(
      "Target A <a href='#cohort-101'>View</a>",
      "Target B <a href='#cohort-102'>View</a>",
      "Outcome A <a href='#cohort-201'>View</a>",
      "Outcome B <a href='#cohort-202'>View</a>",
      "Other <a href='#cohort-999'>View</a>"
    ),
    parentId = c(101, 102, 201, 202, 999),
    parentName = c("Target A", "Target B", "Outcome A", "Outcome B", "Other"),
    subsetId = c(-1, -1, -1, -1, -1),
    isParent = c(TRUE, TRUE, TRUE, TRUE, TRUE),
    subsetName = rep(NA_character_, 5),
    packageVersion = rep(NA_character_, 5),
    numberSubsetOperators = rep(NA_integer_, 5),
    subsetCohorts = rep("", 5),
    appliedSubsets = rep("", 5),
    stringsAsFactors = FALSE
  )
}

make_ci_module_settings <- function() {
  list(
    settings = list(
      irDesign = list(
        targetDefs = list(
          list(id = 101),
          list(id = 102)
        ),
        outcomeDefs = list(
          list(id = 1, cohortId = 201, cleanWindow = 30),
          list(id = 2, cohortId = 202, cleanWindow = 60)
        ),
        timeAtRiskDefs = list(
          list(
            id = 10,
            start = list(dateField = "cohortStartDate", offset = 0),
            end = list(dateField = "cohortEndDate", offset = 30)
          ),
          list(
            id = 11,
            start = list(dateField = "cohortStartDate", offset = 1),
            end = list(dateField = "cohortEndDate", offset = 90)
          )
        ),
        strataSettings = list(
          byAge = TRUE,
          bySex = FALSE,
          minPeople = 1000
        ),
        analysisList = list(
          list(
            targets = c(1, 2),
            outcomes = c(1, 2),
            tars = c(10, 11)
          )
        )
      )
    )
  )
}


test_that("getCountStatement returns expected count sentence", {
  cohort_definition <- make_ci_cohort_definition_df()
  ci_settings <- make_ci_module_settings()

  result <- ProtocolGenerator:::getCountStatement(
    CohortIncidenceModuleSettings = ci_settings,
    cohortDefinitionDf = cohort_definition
  )

  testthat::expect_is(result, "list")
  testthat::expect_equal(length(result), 1)
  testthat::expect_true(grepl("Analysis 1", result[[1]], fixed = TRUE))
  testthat::expect_true(grepl("2 unique parent targets", result[[1]], fixed = TRUE))
  testthat::expect_true(grepl("2 unique parent outcomes with clean windows", result[[1]], fixed = TRUE))
  testthat::expect_true(grepl("2 time-at-risks", result[[1]], fixed = TRUE))
  testthat::expect_true(grepl("Total of 8 T/O/TAR combinations", result[[1]], fixed = TRUE))
})


test_that("createStratSentance returns stratified text when logical flags present", {
  ci_settings <- make_ci_module_settings()

  result <- ProtocolGenerator:::createStratSentance(ci_settings)

  testthat::expect_is(result, "character")
  testthat::expect_true(grepl("Stratified by", result, fixed = TRUE))
  testthat::expect_true(grepl("Age/Sex", result, fixed = TRUE))
})


test_that("createStratSentance returns no stratification text without logical flags", {
  ci_settings <- make_ci_module_settings()
  ci_settings$settings$irDesign$strataSettings <- list(minPeople = 1000, threshold = 0.1)

  result <- ProtocolGenerator:::createStratSentance(ci_settings)

  testthat::expect_equal(result, "No stratification applied.")
})


test_that("getCiTarString formats TAR from definition id", {
  tar_defs <- make_ci_module_settings()$settings$irDesign$timeAtRiskDefs

  result <- ProtocolGenerator:::getCiTarString(tarDefs = tar_defs, tarId = 11)

  testthat::expect_equal(result, "(cohortStartDate + 1) - (cohortEndDate + 90)")
})


test_that("getCiTargetsOutcomes returns expected tables and ids", {
  cohort_definition <- make_ci_cohort_definition_df()
  ci_settings <- make_ci_module_settings()

  result <- ProtocolGenerator:::getCiTargetsOutcomes(
    CohortIncidenceModuleSettings = ci_settings,
    cohortDefinitionDf = cohort_definition
  )

  testthat::expect_is(result, "list")
  testthat::expect_true(all(c("ciTargets", "ciOutcomes", "tars", "ciTargetIds", "ciOutcomeIds") %in% names(result)))

  testthat::expect_equal(length(result$ciTargets), 1)
  testthat::expect_equal(length(result$ciOutcomes), 1)
  testthat::expect_equal(length(result$tars), 1)

  testthat::expect_equal(sort(as.numeric(unlist(result$ciTargetIds))), c(101, 102))
  testthat::expect_equal(sort(as.numeric(unlist(result$ciOutcomeIds))), c(201, 202))

  testthat::expect_equal(nrow(result$ciTargets[[1]]), 2)
  testthat::expect_equal(nrow(result$ciOutcomes[[1]]), 2)
  testthat::expect_true("cleanWindow" %in% colnames(result$ciOutcomes[[1]]))
  testthat::expect_true(grepl("(cohortStartDate + 0) - (cohortEndDate + 30)", result$tars[[1]], fixed = TRUE))
  testthat::expect_true(grepl("(cohortStartDate + 1) - (cohortEndDate + 90)", result$tars[[1]], fixed = TRUE))
})


test_that("getCIcolumns returns expected reactable colDef list", {
  testthat::skip_if_not_installed("reactable")

  result <- ProtocolGenerator:::getCIcolumns()

  testthat::expect_is(result, "list")
  testthat::expect_true(all(c("parentName", "cohortName", "cleanWindow") %in% names(result)))
  testthat::expect_s3_class(result$parentName, "colDef")
  testthat::expect_s3_class(result$cohortName, "colDef")
  testthat::expect_s3_class(result$cleanWindow, "colDef")
})
