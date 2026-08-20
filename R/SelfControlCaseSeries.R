#' getSccsSettings
#'
#' @description
#' Extract exposure/outcomes of interest, shared and analysis specific negative controls, diagnostic settings and analysis settings
#'
#' @details
#' Returns a list of tables and settings
#' 
#' @param SelfControlledCaseSeriesModuleSettings The self controlled case series module specification
#' @param cohortDefinitionDf The data.frame with the cohort definition details
#' @param negativeControls The shared negative controls from the json spec
#' 
#' @return
#' A list with the tables to display
#'
#' @family Extraction
#' @export
#' 
getSccsSettings <- function(
    SelfControlledCaseSeriesModuleSettings,
    cohortDefinitionDf,
    negativeControls
){
  
  if(!is.null(SelfControlledCaseSeriesModuleSettings$settings$sccsAnalysesSpecifications)){
    sccsSettings <- SelfControlledCaseSeriesModuleSettings$settings$sccsAnalysesSpecifications 
  } else{
    sccsSettings <- SelfControlledCaseSeriesModuleSettings$settings
  }
  
  eo <- do.call(
    what = rbind,
    args = lapply(
      X = 1:length(sccsSettings$exposuresOutcomeList),
      FUN = function(k){
        x <- sccsSettings$exposuresOutcomeList[[k]]
        data.frame(
          setting = ifelse(is.null(x$jsonId), 1, x$jsonId), #k,
          outcomeId = rep(x$outcomeId, length(x$exposures)),
          exposureId = unlist(lapply(x$exposures, function(x){x$exposureId})),
          exposureIdRef = unlist(lapply(x$exposures, function(x){paste0(x$exposureIdRef)})),
          nestingId = ifelse(is.null(x$nestingCohortId), -1, x$nestingCohortId),
          trueEffectSize = unlist(lapply(x$exposures, function(x){ifelse(is.null(x$trueEffectSize), '', x$trueEffectSize )}))
        )
        
      }
    )
  )
  
  # ===== NEW FOR COHORT NEGATIVE CONTROLS
  # add code to extract negative controls if missing
  if(is.null(negativeControls)){
    # create negative control data.frame using the exposure outcomes with trueEffectSize == 1
    neg <- do.call(
      what = rbind,
      args = lapply(
        X = 1:length(sccsSettings$exposuresOutcomeList),
        FUN = function(k){
          x <- sccsSettings$exposuresOutcomeList[[k]]
          data.frame(
            cohortId = rep(x$outcomeId, length(x$exposures)),
            outcomeConceptId = rep(x$outcomeId, length(x$exposures)),
            occurrenceType = 'Cohort',
            detectOnDescendants = 'NA',
            trueEffectSize = unlist(lapply(x$exposures, function(x){ifelse(is.null(x$trueEffectSize), '', x$trueEffectSize )}))
          )
        }
      )
    )
    
    neg <- unique(neg[neg$trueEffectSize == 1,])
    negativeControlsSCCS <- merge(neg, cohortDefinitionDf[,c('cohortId', 'cohortName')], by = 'cohortId')
  } else{
    negativeControlsSCCS <- negativeControls
  }
  # ===== END NEW FOR COHORT NEGATIVE CONTROLS
  
  # Should we unique?
  eoUnique <- eo
  #eoUnique <- unique(eo)
  
  # add in the target and indication parents
  tempDf <- cohortDefinitionDf
  colnames(tempDf) <- paste0(colnames(tempDf), 'Target')
  eoUnique <- merge(eoUnique, tempDf, by.x = 'exposureId', by.y = 'cohortIdTarget')
  tempDf <- cohortDefinitionDf[, c('cohortId', 'parentName', 'cohortNameWithLink','cohortName')]
  colnames(tempDf) <- paste0(colnames(tempDf), 'Indication')
  eoUnique <- merge(eoUnique, tempDf, by.x = 'nestingId', by.y = 'cohortIdIndication',
                    all.x = TRUE)
  
  
  # figure out unique negative controls vs shared
  neo <- eoUnique[eoUnique$trueEffectSize == 1 & eoUnique$exposureId != -1,]
  
  # set defaults when there are no negative controls
  negInCommon <- NULL
  negNotInCommon <- NULL
  
  negTabShared <- NULL
  negTab <- NULL
  
  if(nrow(neo) > 0 ){ #if any negative outcomes
    settings <- unique(neo$setting)
    negInCommon <- neo$outcomeId[neo$setting == settings[1]]
    for(ngi in 1:length(settings)){
      negInCommon <- intersect(negInCommon, neo$outcomeId[neo$setting == settings[ngi]])
    }
    negNotInCommon <- neo[!neo$outcomeId %in% negInCommon,]
    
    # if all analyses shared the same negative controls
    if(nrow(negNotInCommon) == 0){
      #eo$setting <- 0
      eoUnique <- eoUnique %>% dplyr::select(-'setting')
    }
    
    if(length(negInCommon)>0){
      negTabShared <- do.call(
        what = rbind,
        args = lapply(
          X = negInCommon, 
          FUN = function(x){negativeControlsSCCS[negativeControlsSCCS$cohortId == x,]}
        )
      )
      
      negTabShared <- negTabShared %>% 
        dplyr::mutate(outcomeName = paste(.data$cohortName, '(concept/cohort: ',.data$outcomeConceptId,')')) %>%
        dplyr::select(-c("cohortId","cohortName","outcomeConceptId"))
      
    } #end if length(negInCommon)>0
    
    
    # add the setting
    if(length(negNotInCommon$outcomeId)>0){
      negTab <- do.call(
        what = rbind,
        args = lapply(
          X = negNotInCommon$outcomeId, 
          FUN = function(x){negativeControlsSCCS[negativeControlsSCCS$cohortId == x,]}
        )
      )
      negTab$cohortId <- unlist(negTab$cohortId)
      negTab <- merge(
        x = negTab,  
        y = negNotInCommon[,c('outcomeId', 'setting','exposureId')], 
        by.x = 'cohortId',
        by.y = 'outcomeId'
      )
      
      tempDf <- cohortDefinitionDf[, c('parentName','cohortName', 'cohortId')]
      colnames(tempDf) <- paste0(colnames(tempDf),'Target')
      negTab <- merge(negTab, tempDf, by.x = 'exposureId', by.y = 'cohortIdTarget')
      
      negTab <- negTab %>% 
        dplyr::mutate(outcomeName = paste(.data$cohortName, '(concept: ',.data$outcomeConceptId,')')) %>%
        dplyr::select(-c("cohortId","cohortName","outcomeConceptId"))
      
    } # end Negtab
    
  } # end if any negative outcomes
  
  eoOfInt <- unique(eoUnique[eoUnique$trueEffectSize == '' & eoUnique$exposureId != -1,])
  tempDf <- cohortDefinitionDf
  colnames(tempDf) <- paste0(colnames(tempDf), 'Outcome')
  eoOfInt <- merge(eoOfInt, tempDf, by.x = 'outcomeId', by.y = 'cohortIdOutcome') %>%
    dplyr::arrange(.data$parentNameTarget, .data$cohortNameIndication, .data$cohortNameOutcome)
  
  
  return(
    list(
      sccsAnalysisList = sccsSettings$sccsAnalysisList,
      sccsDiagnosticThresholds = sccsSettings$sccsDiagnosticThresholds,
      eoOfInt = eoOfInt,
      negTabShared = negTabShared,
      negTab = negTab
    )
  )
}


#' getSccsColDefs
#'
#' @description
#' Create col defs for SCCS
#'
#' @details
#' Returns a named list of colDefs
#' 
#' 
#' @return
#' A column definition list
#'
#' @family ColDefs
#' @export
#' 
getSccsColDefs <- function(){
  res <- list(
    outcomeId = reactable::colDef(show = FALSE),
    exposureId = reactable::colDef(show = FALSE),
    nestingId = reactable::colDef(show = FALSE),
    
    subsetIdTarget = reactable::colDef(show = FALSE),
    isParentTarget = reactable::colDef(show = FALSE),
    parentIdTarget = reactable::colDef(show = FALSE),
    subsetNameTarget = reactable::colDef(show = FALSE),
    packageVersionTarget = reactable::colDef(show = FALSE),
    numberSubsetOperatorsTarget = reactable::colDef(show = FALSE),
    
    subsetIdOutcome = reactable::colDef(show = FALSE),
    isParentOutcome = reactable::colDef(show = FALSE),
    parentIdOutcome = reactable::colDef(show = FALSE),
    subsetNameOutcome = reactable::colDef(show = FALSE),
    packageVersionOutcome = reactable::colDef(show = FALSE),
    numberSubsetOperatorsOutcome = reactable::colDef(show = FALSE),
    
    cohortNameTarget = reactable::colDef(
      show = TRUE,
      name = 'Exposure',
      html = TRUE, 
      filterable = TRUE
    ),
    cohortNameOutcome = reactable::colDef(
      name = 'Outcome',
      html = TRUE,
      filterable = TRUE
    ),
    cohortNameIndication = reactable::colDef(
      name = 'Indication',
      html = TRUE,
      filterable = TRUE
    ),
    
    parentNameOutcome = reactable::colDef(show = FALSE),
    parentNameIndication = reactable::colDef(show = FALSE),
    
    subsetCohortsTarget = reactable::colDef(show = FALSE),
    subsetCohortsOutcome = reactable::colDef(show = FALSE),
    appliedSubsetsTarget = reactable::colDef(show = FALSE),
    appliedSubsetsOutcome = reactable::colDef(show = FALSE),
    
    parentNameTarget = reactable::colDef(
      name = 'Exposure Parent',
      html = TRUE, 
      filterable = TRUE
    ),
    
    cohortNameWithLinkTarget = reactable::colDef(
      show = FALSE,
      name = 'Exposure',
      html = TRUE, 
      filterable = TRUE
    ),
    cohortNameWithLinkOutcome = reactable::colDef(
      show = FALSE,
      name = 'Outcome',
      html = TRUE,
      filterable = TRUE
    ),
    cohortNameWithLinkIndication = reactable::colDef(
      show = FALSE,
      name = 'Indication',
      html = TRUE,
      filterable = TRUE
    )
  )
  return(res)
}
