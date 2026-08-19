#' extractCohortMethodSettings
#'
#' @description
#' Extract cohorts from json
#'
#' @details
#' Returns a names list with the cohorts
#' 
#' @param cohortMethodModuleSettings The cohort method module specification
#' @param negativeControls NULL or a data.frame of the negative controls
#' @param cohortDefinitionDf The data.frame with the cohort definition details
#' 
#' @return
#' An named R list with ...
#'
#' @export
#' 
extractCohortMethodSettings <- function(
    cohortMethodModuleSettings,
    negativeControls,
    cohortDefinitionDf
    ){
  
  if(!is.null(cohortMethodModuleSettings$settings$cmAnalysesSpecifications)){
    settingsCm <- cohortMethodModuleSettings$settings$cmAnalysesSpecifications
  } else{
    settingsCm <- cohortMethodModuleSettings$settings
  }
  
  # if negative controls not in share resources check for any in targetComparatorOutcomesList
  if(is.null(negativeControls)){
    tcoList <- settingsCm$targetComparatorOutcomesList
    cmNeg <- lapply(tcoList, function(x){
      temp <- do.call('rbind',lapply(x$outcomes, function(x2){
        dat <- data.frame(
          cohortId = x2$outcomeId,
          #conceptId = 'NA',
          outcomeConceptId = x2$outcomeId,
          outcomeOfInterest = x2$outcomeOfInterest,
          priorOutcomeLookback = ifelse(is.null(x2$priorOutcomeLookback),0, x2$priorOutcomeLookback)
        )
        # merge with cohortDefinitions
        dat <- merge(dat, cohortDefinitionDf[,c('cohortId', 'cohortName')], by = 'cohortId')
      }))
      temp[!temp$outcomeOfInterest,]
    }
    )
    negativeControlsCM <- unique(do.call(rbind, cmNeg))
  } else{
    negativeControlsCM <- negativeControls
  }
  
  # get tars
  tars <- unique(unlist(lapply(settingsCm$cmAnalysisList, function(x) paste0('\n- (',x$createStudyPopulationArgs$startAnchor, '+' ,x$createStudyPopulationArgs$riskWindowStart, ') - (', x$createStudyPopulationArgs$endAnchor, '+' ,x$createStudyPopulationArgs$riskWindowEnd, ')'))))
  
  
  # do the counts/processing
  tcoList <- settingsCm$targetComparatorOutcomesList
  cmOut <- lapply(tcoList, function(x){
    temp <- do.call('rbind',lapply(x$outcomes, function(x2){
      data.frame(
        outcomeId = x2$outcomeId,
        outcomeOfInterest = x2$outcomeOfInterest,
        priorOutcomeLookback = ifelse(is.null(x2$priorOutcomeLookback),0, x2$priorOutcomeLookback)
      )
    }))
    temp[temp$outcomeOfInterest,]
  }
  )
  cmOutUnique <- unique(cmOut)
  
  cmOutId <- unlist(lapply(cmOut, function(x){which(unlist(lapply(cmOutUnique,function(y) identical(x, y)))) }))
  
  for(cmi in 1:length(cmOutUnique)){
    tempDf <- cohortDefinitionDf
    colnames(tempDf) <- paste0(colnames(cohortDefinitionDf), 'Outcome')
    cmOutUnique[[cmi]] <- merge(cmOutUnique[[cmi]], tempDf, by.x='outcomeId', by.y = 'cohortIdOutcome', all.x = T) %>% 
      dplyr::relocate('cohortNameOutcome') %>%
      dplyr::relocate('parentNameOutcome') %>%
      dplyr::relocate('priorOutcomeLookback', .after = 'cohortNameOutcome') %>% 
      dplyr::arrange(.data$parentNameOutcome, .data$cohortNameOutcome)
  }
  
  
  
  # add cmOutId to target settings
  # add outcome section per cmOutUnique
  
  tcCombos <- do.call(rbind,lapply(1:length(tcoList), function(x){
    nestingId <- ifelse(is.null(tcoList[[x]]$nestingCohortId), -1, tcoList[[x]]$nestingCohortId)
    data.frame(tcoId = x, 
               targetId = tcoList[[x]]$targetId, 
               comparatorId = tcoList[[x]]$comparatorId,
               nestingId = nestingId,
               outcomeSet = paste0("<a href ='#sec-cm-out-",cmOutId[x], "'> Outcome Set ", cmOutId[x], " </a>" )
    )
  }
  )
  )
  
  analysisCm <- settingsCm$cmAnalysisList
  
  # create data.frame with T/C/O with all cohort details 
  tempDf <- cohortDefinitionDf
  colnames(tempDf) <- paste0(colnames(cohortDefinitionDf), 'Target')
  tcCombos<- merge(tcCombos, tempDf, by.x='targetId', by.y = 'cohortIdTarget', all.x = TRUE)
  
  tempDf <- cohortDefinitionDf
  colnames(tempDf) <- paste0(colnames(cohortDefinitionDf), 'Comp')
  tcCombos <- merge(tcCombos, tempDf, by.x='comparatorId', by.y = 'cohortIdComp', all.x = TRUE)
  
  tempDf <- rbind(
    cohortDefinitionDf[, c("cohortId","cohortNameWithLink")],
    c(-1, '')
  )
  colnames(tempDf) <- paste0(colnames(tempDf), 'Nest')
  tcCombos <- merge(tcCombos, tempDf, by.x='nestingId', by.y = 'cohortIdNest', all.x = TRUE)
  
  
  tcCombos$sameSubset <- tcCombos$subsetIdComp == tcCombos$subsetIdTarget
  
  targetParentsCount <- length(unique(tcCombos[, c('parentIdTarget')]))
  targetCohortCount <- length(unique(tcCombos[, c('targetId')]))
  
  compIds <- tcCombos %>%
    dplyr::group_by(.data$targetId) %>%
    dplyr::summarise(
      compCount = length(unique(.data$comparatorId))
    )
  
  
  outcomeRange <- unlist(lapply(cmOutUnique, function(x) nrow(x)))
  if(length(outcomeRange) != 1){
    outcomeRange <- paste0(' between ', min(outcomeRange), ' and ', max(outcomeRange))
  } else{
    outcomeRange <- paste0(min(outcomeRange))
  }
  
  
  # doing excludes and negative controls
  # get all the exclude covariates
  excludeConcepts <- list()
  
  # get all the negative controls
  negative <- list()
  
  for(i in 1:length(settingsCm$targetComparatorOutcomesList)){
    
    # get the excluded concepts
    if(length(settingsCm$targetComparatorOutcomesList[[i]]$excludedCovariateConceptIds) >0){
      excludeConcepts[[i]] <- settingsCm$targetComparatorOutcomesList[[i]]$excludedCovariateConceptIds
    } else{
      excludeConcepts[[i]] <- c(-1)
    }
    
    outcomeCm <- data.frame(
      outcomeId = unlist(lapply(settingsCm$targetComparatorOutcomesList[[i]]$outcomes, function(x) x$outcomeId)),
      outcomeOfInterest = unlist(lapply(settingsCm$targetComparatorOutcomesList[[i]]$outcomes, function(x) x$outcomeOfInterest)),
      priorOutcomeLookback = unlist(lapply(settingsCm$targetComparatorOutcomesList[[i]]$outcomes, function(x) ifelse(is.null(x$priorOutcomeLookback), 0,  x$priorOutcomeLookback)))
    )
    
    #outcomes <- outcomeCm[outcomeCm$outcomeOfInterest,c('outcomeId','priorOutcomeLookback')]
    negative[[i]] <- outcomeCm[!outcomeCm$outcomeOfInterest,c('outcomeId','priorOutcomeLookback')]
  }
  
  # now process the negative list and excludeConcepts list
  commonExclude <- excludeConcepts[[1]]
  if(length(excludeConcepts) > 1){
    if(commonExclude[1] != -1){ 
      for(ind in 2:length(excludeConcepts)){
        commonExclude <- intersect(excludeConcepts[[ind]],commonExclude) 
      }
    } else{
      commonExclude <- c()
    }
  }
  nonCommonExclude <- lapply(excludeConcepts, function(x) setdiff(x, commonExclude))
  nonCommonSets <- unique(nonCommonExclude)
  nonZero <- unlist(lapply(nonCommonExclude, function(x) length(x)>0))
  if(sum(nonZero) > 0){ 
    excludeSetId <- unlist(lapply(nonCommonExclude, function(x) which(unlist(lapply(1:length(nonCommonSets), function(ind) identical(x, nonCommonSets[[ind]]))))))
  } else{
    excludeSetId <- rep(NA, length(excludeConcepts))
  }
  
  commonNegativeId <- negative[[1]]$outcomeId
  if(length(negative) > 1){
    for(ind in 2:length(negative)){
      commonNegativeId <- intersect(commonNegativeId, negative[[ind]]$outcomeId)
    } 
  }
  nonCommonNegative <- lapply(negative, function(x) setdiff(x$outcomeId, commonNegativeId))
  nonCommonNegSets <- unique(nonCommonNegative)
  nonZero <- unlist(lapply(nonCommonNegSets, function(x) length(x)>0))
  if(sum(nonZero) > 0){ # finished here friday
    negSetId <- unlist(lapply(nonCommonNegative, function(x) which(unlist(lapply(1:length(nonCommonNegSets), function(ind) identical(x, nonCommonNegSets[[ind]]))))))
  } else{
    negSetId <- rep(0, length(negative))
  }
  
  # sets: nonCommonSets -- nonCommonNegSets
  # add additional negative control and exclude set ids to tco
  
  # add to tcCombos
  tcCombos <- merge(
    tcCombos,  
    data.frame(
      tcoId = 1:length(excludeSetId),
      additionalExclusions = paste0("<a href ='#sec-cm-exclude-",excludeSetId,"'> View </a>")
    ),
    by = 'tcoId'
  )
  
  tcCombos <- merge(
    tcCombos,  
    data.frame(
      tcoId = 1:length(negSetId),
      additionalNegativeControlId = paste0("<a href ='#sec-cm-negset-",negSetId,"'> View </a>")
    ),
    by = 'tcoId'
  )
  
  if(!'subsetCohortsTarget' %in% colnames(tcCombos)){
    tcCombos$subsetCohortsTarget <- ''
  }
  if(!'appliedSubsetsTarget' %in% colnames(tcCombos)){
    tcCombos$appliedSubsetsTarget <- ''
  }
  
  tcCombos <- tcCombos %>% 
    dplyr::relocate('cohortNameWithLinkNest') %>%
    dplyr::relocate('cohortNameTarget') %>%
    dplyr::relocate('cohortNameComp') %>%
    dplyr::relocate('parentNameTarget') %>%
    dplyr::relocate('sameSubset', .after = 'cohortNameWithLinkNest') %>%
    dplyr::relocate('subsetCohortsTarget', .after = 'sameSubset') %>%
    dplyr::relocate('appliedSubsetsTarget', .after = 'subsetCohortsTarget') %>%
    dplyr::relocate('additionalExclusions', .after = 'appliedSubsetsTarget') %>%
    dplyr::relocate('additionalNegativeControlId', .after = 'additionalExclusions') %>%
    dplyr::relocate('outcomeSet', .after = 'cohortNameWithLinkNest') %>% 
    dplyr::arrange(.data$parentNameTarget, .data$cohortNameComp, .data$cohortNameTarget)
  
  # extract diagnostics
  diagSetting <- NULL
  if('cmDiagnosticThresholds' %in% names(settingsCm)){
    diagSetting <- settingsCm$cmDiagnosticThresholds
  }


return(
  list(

    negativeControlsCM  = negativeControlsCM,
    tcCombos = tcCombos,
    outcomeRange = outcomeRange,
    compIds = compIds,
    targetParentsCount = targetParentsCount,
    targetCohortCount = targetCohortCount,
    tars = tars,
    analysisCm = analysisCm,
    
    cmOutUnique = cmOutUnique,
    
    commonExclude = commonExclude,
    nonCommonSets = nonCommonSets,
    commonNegativeId = commonNegativeId,
    nonCommonNegSets = nonCommonNegSets,
    
    diagSetting = diagSetting,
    
    refitPsForEveryOutcome = settingsCm$refitPsForEveryOutcome,
    refitPsForEveryStudyPopulation = settingsCm$refitPsForEveryStudyPopulation
    
  )
)

}


#' cmColDef
#'
#' @description
#' Extract cohorts from json
#'
#' @details
#' Returns a names list with the cohorts
#' 
#' @param elementId An element id for the table using this column definitions (needed for the drop down selection)
#' @param colNames Optional a vector of column names to restrict to
#' 
#' @return
#' A column definition list
#'
#' @export
#' 
cmColDef <- function(
    elementId = "cm-tc-tab",
    colNames = NULL
    ){
  
  colDef <- list(
    parentNameTarget = reactable::colDef(
      name = 'Target Parent',
      html = TRUE, 
      filterable = TRUE, 
      minWidth = 300
    ),
    cohortNameComp = reactable::colDef(
      aggregate = "count",
      show = TRUE,
      name = 'Comparator',
      filterable = TRUE, 
      minWidth = 300
    ),
    cohortNameTarget = reactable::colDef(
      show = TRUE,
      name = 'Target',
      filterable = TRUE, 
      minWidth = 300
    ),
    additionalExclusions = reactable::colDef(
      html = TRUE,
      filterable = TRUE
    ),
    additionalNegativeControlId = reactable::colDef(
      html = TRUE,
      filterable = TRUE
    ),
    sameSubset = reactable::colDef(
      filterable = TRUE,
      filterInput = function(values, name) {
        shiny::tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", elementId, name),
          # "All" has an empty value to clear the filter, and is the default option
          shiny::tags$option(value = "", "All"),
          lapply(unique(values), shiny::tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    ),
    outcomeSet = reactable::colDef(
      show = TRUE, 
      html = TRUE
    ),
    subsetCohortsTarget = reactable::colDef(
      show = TRUE, 
      html = TRUE
    ),
    appliedSubsetsTarget = reactable::colDef(
      show = TRUE, 
      html = TRUE
    ),
    cohortNameWithLinkTarget = reactable::colDef(
      show = FALSE,
      name = 'Target',
      html = TRUE, 
      filterable = TRUE
    ),
    cohortNameWithLinkComp = reactable::colDef(
      show = FALSE,
      name = 'Comparator',
      html = TRUE,
      filterable = TRUE
    ),
    cohortNameWithLinkNest = reactable::colDef(
      show = TRUE,
      name = 'Nesting Cohort',
      html = TRUE,
      filterable = TRUE
    ),
    nestingId = reactable::colDef(show = FALSE), 
    tcoId = reactable::colDef(show = FALSE),
    comparatorId = reactable::colDef(show = FALSE),
    targetId = reactable::colDef(show = FALSE),
    subsetIdTarget = reactable::colDef(show = FALSE),
    isParentTarget = reactable::colDef(show = FALSE),
    parentIdTarget = reactable::colDef(show = FALSE),
    subsetNameTarget = reactable::colDef(show = FALSE),
    packageVersionTarget = reactable::colDef(show = FALSE),
    numberSubsetOperatorsTarget = reactable::colDef(show = FALSE),
    subsetIdComp = reactable::colDef(show = FALSE),
    isParentComp = reactable::colDef(show = FALSE),
    parentIdComp = reactable::colDef(show = FALSE),
    subsetNameComp = reactable::colDef(show = FALSE),
    packageVersionComp = reactable::colDef(show = FALSE),
    numberSubsetOperatorsComp = reactable::colDef(show = FALSE),
    cohortNameTarget = reactable::colDef(show = FALSE),
    parentNameComp = reactable::colDef(show = FALSE),
    subsetCohortsComp = reactable::colDef(show = FALSE),
    appliedSubsetsComp = reactable::colDef(show = FALSE)
    
  )
  
  # restrict to colNames
  if(!is.null(colNames)){
    colDef <- colDef[names(colDef) %in% colNames]
  }
  
  return(colDef)
}


#' cmOutcomeColDef
#'
#' @description
#' List with column names for the cohort method outcome table
#'
#' @details
#' Returns a names list with the cohorts names
#' 
#' @param colNames Optional a vector of column names to restrict to
#' 
#' @return
#' A column definition list
#'
#' @export
#'
#'
cmOutcomeColDef <- function(
    colNames
    )
  {

  colDefs <- list(
  priorOutcomeLookback = reactable::colDef(
    aggregate = "unique",
    show = TRUE,
    name = 'Prior Outcome Lookback (days)',
    filterable = TRUE
  ),
  outcomeId = reactable::colDef(show = FALSE),
  outcomeOfInterest = reactable::colDef(show = FALSE),
  subsetIdOutcome = reactable::colDef(show = FALSE),
  parentNameOutcome = reactable::colDef(
    show = TRUE,
    name = 'Outcome Parent',
    filterable = TRUE,
    html = TRUE, 
    minWidth = 300
  ),
  cohortNameOutcome = reactable::colDef(
    show = TRUE,
    name = 'Outcome',
    html = TRUE,
    filterable = TRUE, 
    minWidth = 300
  ),
  cohortNameWithLinkOutcome = reactable::colDef(
    show = FALSE,
    name = 'Outcome',
    html = TRUE,
    filterable = TRUE
  ),
  isParentOutcome = reactable::colDef(show = F),
  parentIdOutcome = reactable::colDef(show = F),
  subsetNameOutcome = reactable::colDef(show = F),
  packageVersionOutcome = reactable::colDef(show = F),
  numberSubsetOperatorsOutcome = reactable::colDef(show = F),
  subsetCohortsOutcome = reactable::colDef(show = F),
  appliedSubsetsOutcome = reactable::colDef(show = F)
)
  
  colDefs <- colDefs[names(colDefs) %in% colNames]
  
  return(colDefs)
}
