#' getPlpSettings
#'
#' @description
#' Extract plp tables and settings from json
#'
#' @details
#' Returns a names list with the tables and settings
#' 
#' @param PatientLevelPredictionModuleSettings The patient level prediction module specification
#' @param cohortDefinitionDf The data.frame with the cohort definition details
#' 
#' @return
#' An named R list with ...
#'
#' @family Extraction
#' @export
#' 
getPlpSettings <- function(
    PatientLevelPredictionModuleSettings,
    cohortDefinitionDf
){
  
  tos <- data.frame(
    targetId =  unlist(lapply(PatientLevelPredictionModuleSettings$settings$modelDesignList, function(x) x$targetId)),
    outcomeId = unlist(lapply(PatientLevelPredictionModuleSettings$settings$modelDesignList, function(x) x$outcomeId))
  )
  
  # add names, parents, subset info for t and o
  tempDf <- cohortDefinitionDf
  colnames(tempDf) <- paste0(colnames(tempDf),'Target')
  tos <- merge(tempDf, tos, by.x = 'cohortIdTarget', by.y = 'targetId')
  tempDf <- cohortDefinitionDf
  colnames(tempDf) <- paste0(colnames(tempDf),'Outcome')
  tos <- merge(tempDf, tos, by.x = 'cohortIdOutcome', by.y = 'outcomeId')
  
  
  # remove T and O from model designs and get unique
  # may need to also remove seed from split?
  modelDesign <- PatientLevelPredictionModuleSettings$settings$modelDesignList
  for(i in 1:length(modelDesign)){
    modelDesign[[i]]$targetId <- NULL
    modelDesign[[i]]$outcomeId <- NULL
  }
  modelDesignUnique <- unique(modelDesign)
  
  tos$designId <- rep(0, length(modelDesign))
  for(j in 1:length(modelDesignUnique)){
    tos$designId[which(unlist(lapply(modelDesign, function(x) identical(modelDesignUnique[[j]], x))))] <- j
  }
  
  # covariate set - get attr(,"fun")
  covSet <- c()
  for(cind in 1:length(modelDesignUnique)){
    if(inherits(modelDesignUnique[[cind]]$covariateSettings, 'covariateSettings')){
      modelDesignUnique[[cind]]$covariateSettings <- list(modelDesignUnique[[cind]]$covariateSettings)
    }
    
    covSet <- c(covSet,paste0(unlist(lapply(modelDesignUnique[[cind]]$covariateSettings, function(x){
      func <- attr(x, "fun")
      settings <- x[sapply(x, function(x) is.logical(x))]
      if(length(settings)>0){
        settings <- names(settings)[unlist(settings)]
        func <- paste0(c(func, paste0(settings, collapse = ',')), collapse = ': ')
      }
      return(func)
    })), collapse = ' - '))
    
  }
  
  # TODO remove this or revise? - add covariate summary name?
  predictionSummary <- data.frame(
    model_design = paste0("<a href='#sec-model-design-",1:length(modelDesignUnique),"'> View </a>"),
    number_targets = unlist(lapply(1:length(modelDesignUnique), function(x){length(unique(tos$parentIdTarget[tos$designId == x]))})),
    number_targets_with_subsets = unlist(lapply(1:length(modelDesignUnique), function(x){length(unique(tos$cohortIdTarget[tos$designId == x]))})),
    number_outcomes = unlist(lapply(1:length(modelDesignUnique), function(x){length(unique(tos$parentIdOutcome[tos$designId == x]))})),
    number_outcomes_with_subsets = unlist(lapply(1:length(modelDesignUnique), function(x){length(unique(tos$cohortIdOutcome[tos$designId == x]))})),
    timeAtRisk = paste0(
      unlist(lapply(modelDesignUnique , function(x) x$populationSettings$startAnchor)), 
      ' + ',
      unlist(lapply(modelDesignUnique, function(x) x$populationSettings$riskWindowStart)),
      ' - ',
      unlist(lapply(modelDesignUnique, function(x) x$populationSettings$endAnchor)), 
      ' + ',
      unlist(lapply(modelDesignUnique, function(x) x$populationSettings$riskWindowEnd))
    ),
    covariates = covSet
  )
  
  return(
    list(
      targetOutcomeSet = tos,
      modelDesignUnique = modelDesignUnique,
      predictionSummary = predictionSummary
    )
  )
  
}


#' getPlpColDefs
#'
#' @description
#' create colDefs for prediction table
#'
#' @details
#' Returns a names list with the cohorts
#' 
#' 
#' @return
#' A column definition list
#'
#' @family ColDefs
#' @export
#' 
getPlpColDefs <- function(){
  res <- list(
    model_design = reactable::colDef(
      html = TRUE, 
      name = 'Model Design' 
    ),
    number_targets = reactable::colDef(
      name = 'Parent Target Count' 
    ),
    number_targets_with_subsets = reactable::colDef(
      name = 'Target Count' 
    ),
    number_outcomes = reactable::colDef(
      name = 'Parent Outcome Count' 
    ),
    number_outcomes_with_subsets = reactable::colDef(
      name = 'Outcome Count' 
    ),
    timeAtRisk = reactable::colDef(
      name = 'Time-at-risk',
      width = 200
    ),
    covariates = reactable::colDef(
      name = 'Covariate Set', 
      width = 300,
      cell = function(value) {
        # Truncate to 20 characters and optionally add '...'
        paste0(substr(value, 1, 50), '...')
      }#,
      # Define what shows when expanded
      #details = function(index) {
      #  shiny::div(style = "padding: 10px;", data$covariates[index])
      #}
    )
    
  )
  
  return(res)
}

