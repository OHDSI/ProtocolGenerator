#' getCountStatement
#'
#' @description
#' Create a sentence that explains the number of targets, outcomes and settings per analysis.
#'
#' @details
#' Returns a string 
#' 
#' @param CohortIncidenceModuleSettings The cohort incidence module specification
#' @param cohortDefinitionDf The data.frame with the cohort definition details
#' 
#' @return
#' An string with the count information
#' 
#' @family Extraction
#' @export
#' 
getCountStatement <- function(
    CohortIncidenceModuleSettings,
    cohortDefinitionDf
    ){
  
  allCounts <- lapply(
    CohortIncidenceModuleSettings$settings$irDesign$analysisList, function(x){
      targetIds <- x$targets
      outcomeIds <- x$outcomes
      tars <- x$tars
      
      #updated for new CI
      targetIds <- unlist(lapply(1:length(targetIds), function(ind) CohortIncidenceModuleSettings$settings$irDesign$targetDefs[[ind]]$id))
      outcomeIds <- unlist(lapply(1:length(outcomeIds), function(ind) CohortIncidenceModuleSettings$settings$irDesign$outcomeDefs[[ind]]$cohortId))
      outcomeCleanWindow <- unlist(lapply(1:length(outcomeIds), function(ind) CohortIncidenceModuleSettings$settings$irDesign$outcomeDefs[[ind]]$cleanWindow))
      parentIdsOutcome <-  unlist(lapply(1:length(outcomeIds), function(ind){ 
        outcomeId <- CohortIncidenceModuleSettings$settings$irDesign$outcomeDefs[[ind]]$cohortId;
        parentId <- cohortDefinitionDf$parentId[cohortDefinitionDf$cohortId %in% outcomeId]
      }))
      
      counts <- list(
        cohortTs = length(unique(targetIds)),
        parentTs = length(unique(cohortDefinitionDf$parentId[cohortDefinitionDf$cohortId %in% targetIds])),
        cohortOs = nrow(unique(cbind(outcomeIds, outcomeCleanWindow))),
        parentOs = nrow(unique(cbind(parentIdsOutcome, outcomeCleanWindow))),
        parentOsNoWindow = length(unique(parentIdsOutcome)),
        tars = length(unique(tars)),
        total = length(unique(targetIds))*nrow(unique(cbind(outcomeIds, outcomeCleanWindow)))*length(unique(tars))
      )
      return(counts)
    })
  
  totarCounts <- do.call(sum, lapply(CohortIncidenceModuleSettings$settings$irDesign$analysisList, function(x){length(x[[1]])*length(x[[2]])*length(x[[3]])}))
  analysisCount <- length(CohortIncidenceModuleSettings$settings$irDesign$analysisList)
  targetCount <- length(CohortIncidenceModuleSettings$settings$irDesign$targetDefs)
  outcomeCount <- length(CohortIncidenceModuleSettings$settings$irDesign$outcomeDefs)
  tarCount <- length(CohortIncidenceModuleSettings$settings$irDesign$timeAtRiskDefs)
  totarCountsUnique <-sum(unlist(lapply(allCounts, function(x) x$total)))
  
  # create count sentances for each analysis
  countSentances <- lapply(
    X = 1:length(allCounts), 
    FUN = function(i){
      paste0('- Analysis ', i, ' @sec-incidence-analysis-',i,' : ', 
             allCounts[[i]]$parentTs, ' unique parent targets (',allCounts[[i]]$cohortTs,
             ' unique target subsets) see @sec-incidence-t-',i,', ', allCounts[[i]]$parentOs, 
             ' unique parent outcomes with clean windows (',allCounts[[i]]$cohortOs,
             ' unique outcome subsets with clean windows and ',allCounts[[i]]$parentOsNoWindow,
             ' unique parent outcomes see @sec-incidence-o-',i,') and ',allCounts[[i]]$tars, 
             ' time-at-risks see @sec-incidence-tar-',i,'. Total of ', allCounts[[i]]$total, 
             ' T/O/TAR combinations in analysis ',i,'.')
    }
  )
  
  return(countSentances)
}

#' createStratSentance
#'
#' @description
#' Create a sentence that explains the stratification defined in the analysis.
#'
#' @details
#' Returns a string 
#' 
#' @param CohortIncidenceModuleSettings The cohort incidence module specification
#' 
#' @return
#' An string with the stratification information
#'
#' @family Extraction
#' @export
#' 
createStratSentance <- function(CohortIncidenceModuleSettings){
  stratInd <- unlist(lapply(CohortIncidenceModuleSettings$settings$irDesign$strataSettings, function(x) is.logical(x)))
  
  if(sum(stratInd)>0){
    stratSentance <- paste0('Stratified by ', paste0(gsub('by', '', names(stratInd)[stratInd]), collapse = '/'), ', see @sec-incidence-strat. There will be more results due to including stratification of the target cohorts.')
  } else{
    stratSentance <- 'No stratification applied.'
  }
  
  return(stratSentance)
}


#' getCiTargetsOutcomes
#'
#' @description
#' Extracts a list of target tables, outcome tables, TARs and vector of unique targetIds and outcomeIds in analysis
#'
#' @details
#' Returns a list
#' 
#' @param CohortIncidenceModuleSettings The cohort incidence module specification
#' @param cohortDefinitionDf The data.frame with the cohort definition details
#' 
#' @return
#' A list with the tables to present in the protocol
#'
#' @family Extraction
#' @export
#' 
getCiTargetsOutcomes <- function(
    CohortIncidenceModuleSettings, 
    cohortDefinitionDf
){
  
  tarDefs <- CohortIncidenceModuleSettings$settings$irDesign$timeAtRiskDefs
  
  tars <- list()
  ciTargets <- list()
  ciOutcomes <- list()
  ciTargetIds <- c()
  ciOutcomeIds <- c()
  
  for(i in 1:length(CohortIncidenceModuleSettings$settings$irDesign$analysisList)){
    
    targets <- CohortIncidenceModuleSettings$settings$irDesign$analysisList[[i]]$targets
    targetIds <- unlist(lapply(1:length(targets), function(id){
      CohortIncidenceModuleSettings$settings$irDesign$targetDefs[[id]]$id
    }))
    
    ciTargetIds <- c(ciTargetIds, targetIds) # vector or all targets
    
    # this is now the ci cohort ids not the main cohort ids
    outcomeIdsCI <- CohortIncidenceModuleSettings$settings$irDesign$analysisList[[i]]$outcomes
    # create lookup to find outcome ids
    outcomeLookup <- as.data.frame(
      do.call(
        rbind, 
        CohortIncidenceModuleSettings$settings$irDesign$outcomeDefs)
    )
    outcomeDf <- outcomeLookup[outcomeLookup$id %in% outcomeIdsCI,c('cohortId', 'cleanWindow')]
    
    ciOutcomeIds <- c(ciOutcomeIds,outcomeLookup$cohortId) # vector of outcomes
    
    # The nicely formatted settings
    ciTargets[[i]] <- cohortDefinitionDf[cohortDefinitionDf$cohortId %in% targetIds,]
    
    ciOutcomes[[i]] <- merge(
      x = cohortDefinitionDf, 
      y = outcomeDf, 
      by = 'cohortId'
    ) %>%
      dplyr::relocate("cleanWindow", .after = "parentName")
    
    
    tars[[i]] <- paste0(' - ', paste0(sapply(
      X = CohortIncidenceModuleSettings$settings$irDesign$analysisList[[i]]$tars, 
      FUN = function(x){getCiTarString(tarDefs, x)}), 
      collapse=' \n - '))
  }
  
  
  return(
    list(
      ciTargets = ciTargets, # list of cohortDefinitionDf restricted targetIds in study
      ciOutcomes = ciOutcomes, # list of cohortDefinitionDf restricted outcomeIds in study
      tars = tars, # list of tar setting from CohortIncidenceModuleSettings
      ciTargetIds = unique(ciTargetIds), # vector of all target ids used in CI
      ciOutcomeIds = unique(ciOutcomeIds) # vector of all outcome ids used in CI
    )
  ) 
}

# helper for getCiTargetsOutcomes
getCiTarString <- function(tarDefs, tarId){
  res <- tarDefs[[which(unlist(lapply(tarDefs, function(x) x$id)) == tarId)]]
  
  return(paste0('(',res$start$dateField ,' + ', res$start$offset, ') - (', res$end$dateField, ' + ', res$end$offset, ')'))
}


#' getCIcolumns
#'
#' @description
#' A reactable colDef list for the cohort incidence tables 
#'
#' @details
#' Returns a list of colDefs
#' 
#' 
#' @return
#' A list of colDefs for the target and outcome tables describing the cohort incidence analysis
#'
#' @family ColDefs
#' @export
#' 
getCIcolumns <- function(){
  ciColumns <- list(
    subsetId = reactable::colDef(show = FALSE),
    isParent = reactable::colDef(show = FALSE),
    parentId = reactable::colDef(show = FALSE),
    subsetName = reactable::colDef(show = FALSE),
    packageVersion = reactable::colDef(show = FALSE),
    numberSubsetOperators = reactable::colDef(show = FALSE),
    cohortId  = reactable::colDef(show = FALSE),
    parentName = reactable::colDef(
      name = 'Parent Cohort', 
      defaultSortOrder = 'asc',
      sortNALast = TRUE,
      filterable = TRUE, 
      minWidth = 300,
      html = TRUE
    ),
    cohortName = reactable::colDef(
      show = TRUE,
      #aggregate = "unique",
      name = 'Cohort', 
      defaultSortOrder = 'asc',
      sortNALast = TRUE,
      filterable = TRUE, 
      minWidth = 300
    ),
    cohortNameWithLink = reactable::colDef(
      show = FALSE,
      name = 'Cohort', 
      defaultSortOrder = 'asc',
      sortNALast = TRUE,
      filterable = TRUE, 
      minWidth = 300,
      html = TRUE
    ),
    subsetCohorts = reactable::colDef(
      #aggregate = "count",
      html = TRUE
    ),
    appliedSubsets = reactable::colDef(
      #aggregate = "count",
      html = TRUE
    ),
    cleanWindow =  reactable::colDef(
      #aggregate = "unique",
      filterable = TRUE,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('ci-out-tab', '%s', event.target.value || undefined)", name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    )
  )
  return(ciColumns)
}