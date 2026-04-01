#' getCohortDiagnosticTables
#'
#' @description
#' Extract target, setting and feature table from CohortDiagnosticsSettings
#'
#' @details
#' Returns a list of tables
#' 
#' @param CohortDiagnosticsSettings The cohort diagnostic module specification
#' @param cohortDefinitionDf The data.frame with the cohort definition details
#' 
#' @return
#' A list with the tables to display
#'
#' @export
#' 
getCohortDiagnosticTables <- function(
  CohortDiagnosticsSettings,
  cohortDefinitionDf
){
  
  cohortIdsCD <- CohortDiagnosticsSettings$cohortIds
  # if null then cohort diagnotics is applied to all cohorts in set
  if(is.null(cohortIdsCD)){
    cohortIdsCD <- cohortDefinitionDf$cohortId
  }
  
  # this should be a single settings rather than a list of settings
  temporalCovariateSettingsCD <- CohortDiagnosticsSettings$temporalCovariateSettings
  featureTable <- data.frame(
    input = names(temporalCovariateSettingsCD),
    value = unlist(lapply(temporalCovariateSettingsCD, FUN = function(x) ifelse(is.null(x), 'NULL', as.character(x))))
  )
  rownames(featureTable) <- NULL
  
  # non list settings
  cdSettings <- CohortDiagnosticsSettings
  cdSettings$cohortIds <- NULL
  cdSettings$temporalCovariateSettings <- NULL
  
  settingsTable <- data.frame(
    input = names(cdSettings),
    value = unlist(lapply(cdSettings, FUN = function(x) ifelse(is.null(x), 'NULL', as.character(x))))
  )
  rownames(settingsTable) <- NULL
  
  # create target table
  targetTable <- cohortDefinitionDf
  colnames(targetTable) <- paste0(colnames(targetTable), 'Target')
  targetTable <- targetTable[targetTable$cohortIdTarget %in% cohortIdsCD, ]
  
  # order the columns
  targetTable <- targetTable  %>% 
    dplyr::relocate("cohortNameTarget") %>% 
    dplyr::relocate("parentNameTarget") 
  
  return(
    list(
      targetTable = targetTable,
      settingsTable = settingsTable,
      featureTable = featureTable
    )
  )
}


#' getCdCols
#'
#' @description
#' A list of colDefs for the columns used in cohort diagnostic protocol module
#'
#' @details
#' Returns a list of colDefs
#' 
#' 
#' @return
#' A list of colDefs for cohort diagnostic 
#'
#' @export
#' 
getCdCols <- function(){
  res <- list(
    subsetIdTarget = reactable::colDef(show = F),
    isParentTarget = reactable::colDef(show = F),
    parentIdTarget = reactable::colDef(show = F),
    subsetNameTarget = reactable::colDef(show = F),
    packageVersionTarget = reactable::colDef(show = F),
    numberSubsetOperatorsTarget = reactable::colDef(show = F),
    cohortIdTarget = reactable::colDef(show = F),
    
    parentNameTarget = reactable::colDef(
      name = 'Parent Target', 
      defaultSortOrder = 'asc',
      sortNALast = TRUE,
      filterable = TRUE, 
      minWidth = 300,
      html = TRUE
    ),
    
    cohortNameTarget = reactable::colDef(
      aggregate = "unique",
      show = TRUE,
      name = 'Target', 
      defaultSortOrder = 'asc',
      sortNALast = TRUE,
      filterable = TRUE, 
      minWidth = 300
    ),
    cohortNameWithLinkTarget = reactable::colDef(
      show = FALSE,
      name = 'Cohort', 
      defaultSortOrder = 'asc',
      sortNALast = TRUE,
      filterable = TRUE, 
      minWidth = 300,
      html = TRUE
    ),
    subsetCohortsTarget = reactable::colDef(
      html = TRUE,
      aggregate = "count"
    ),
    appliedSubsetsTarget = reactable::colDef(
      html = TRUE,
      aggregate = "count"
    )
  )
  return(res)
}