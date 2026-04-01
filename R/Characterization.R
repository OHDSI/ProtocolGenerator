globalCharacterizationSettings <- function(
  CharacterizationModuleSettings
){
  
  txt <- paste0(
    'Only covariates that occur >= ',
    CharacterizationModuleSettings$settings$minCharacterizationMean,
    ' fraction of the population and >= ', 
    CharacterizationModuleSettings$settings$minCovariateCount,
    ' people are returned. ',
    'The risk factor analysis used mode ', 
    CharacterizationModuleSettings$settings$mode,
    ' and only returns covariates where the absolute SMD is >= ',
    CharacterizationModuleSettings$settings$minSMD,
    '.  All cohorts created by Characterization will be saved into ',
    CharacterizationModuleSettings$settings$outputTable,
    ' within the Strategus work schema.'
  )

   return(txt)
}

processTar <- function(
  riskWindowStart, 
  startAnchor,
  riskWindowEnd, 
  endAnchor
){
  
  text <- paste0('(',
         startAnchor, '+', riskWindowStart, 
         ') - (',
         endAnchor, '+', riskWindowEnd
         ,')')
  
  return(text)
}



processTargetBaseineSettings <- function(
    CharacterizationModuleSettings,
    cohortDefinitionDf # process cohortDefinition 
    ){
  
  tbSpec <- CharacterizationModuleSettings$settings$analysis$targetBaselineSettings
  
  if(is.null(tbSpec)){
    return(NULL)
  }
    
  # get the unique covariate settings
  covariateJson <- lapply(
    X = tbSpec, 
    FUN = function(x) ParallelLogger::convertSettingsToJson(x$covariateSettings)
  )
  covariateJsonUnique <- unique(covariateJson)
  
  # extract: target_id, limitToFirstInNDays, minPriorObservation, covariateSettingId
  targetSettings <- do.call(rbind, lapply(
    X = tbSpec, 
    FUN = function(x) data.frame(
      targetId = x$targetIds, 
      limitToFirstInNDays = x$limitToFirstInNDays,
      minPriorObservation = x$minPriorObservation,
      covariateSettingId = match(ParallelLogger::convertSettingsToJson(x$covariateSettings),covariateJsonUnique)
    )
  )
  )
  
  # create target table
  tempDf <- cohortDefinitionDf
  colnames(tempDf) <- paste0(colnames(tempDf), 'Target')
  targetDf <- merge(targetSettings, tempDf, by.x = 'targetId', by.y = 'cohortIdTarget')
  targetDf$setting <- paste0("<a href ='#sec-char-tb-setting-",targetDf$covariateSettingId,"'>Setting ",targetDf$covariateSettingId,"</a>")
  
  # order the columns
  targetDf <- targetDf %>% 
    dplyr::relocate("cohortNameTarget") %>% 
    dplyr::relocate("parentNameTarget") %>% 
    dplyr::relocate("limitToFirstInNDays", .after = "cohortNameTarget") %>%
    dplyr::relocate("minPriorObservation", .after = "limitToFirstInNDays") %>%
    dplyr::relocate("setting", .after = dplyr::last_col())
  
  return(
    list(
      tableData = targetDf,
      settingsJson = covariateJsonUnique 
    )
  )
}



processRiskFactorSettings <- function(
    CharacterizationModuleSettings,
    cohortDefinitionDf # process cohortDefinition 
    ){
  
  rfSpec <- CharacterizationModuleSettings$settings$analysis$riskFactorSettings
  
  if(is.null(rfSpec)){
    return(NULL)
  }
  
  # get the unique covariate settings
  covariateJsonUnique <- unique(lapply(
    X = rfSpec, 
    FUN = function(x) ParallelLogger::convertSettingsToJson(x$covariateSettings)
  ))
  
  # get unique outcome sets
  rfOutcomeList <- unique(lapply(
    X = rfSpec, 
    FUN = function(x) {
      data.frame(
        outcomeId = x$outcomeIds,
        outcomeWashoutDays = x$outcomeWashoutDays,
        tar = processTar(
          riskWindowStart = x$riskWindowStart, 
          startAnchor = x$startAnchor,
          riskWindowEnd = x$riskWindowEnd, 
          endAnchor = x$endAnchor
        )
      )
    }))
  
  # get unique target sets
  rfTargetList <- unique(lapply(
    X = rfSpec, 
    FUN = function(x) {
      data.frame(
        targetId = x$targetIds, # need to account for multiple targetIds
        limitToFirstInNDays = x$limitToFirstInNDays,
        minPriorObservation = x$minPriorObservation
      )
    }
  ))
  
  if(length(rfTargetList) == 1){
    # all outcomes by all targets 
    rfTargetSettings <- unique(do.call(rbind, lapply(
      X = rfSpec, 
      FUN = function(x) {
        
        data.frame(
          targetId = x$targetIds, # need to account for multiple targetIds
          limitToFirstInNDays = x$limitToFirstInNDays,
          minPriorObservation = x$minPriorObservation,
          outcomeSet = 1,
          covariateSettingId = match(ParallelLogger::convertSettingsToJson(x$covariateSettings),covariateJsonUnique)
        )
        
      }
    )
    ))
    
    rfOutcomeList <- list(do.call(rbind, rfOutcomeList))
    
  } else{
    
    # extract: target_id, limitToFirstInNDays, minPriorObservation, covariateSettingId
    rfTargetSettings <- unique(do.call(rbind, lapply(
      X = rfSpec, 
      FUN = function(x) {
        
        data.frame(
          targetId = x$targetIds, # need to account for multiple targetIds
          limitToFirstInNDays = x$limitToFirstInNDays,
          minPriorObservation = x$minPriorObservation,
          outcomeSet = which(unlist(lapply(
            X = rfOutcomeList,
            FUN = function(y){identical(y, data.frame(outcomeId = x$outcomeIds,
                                                      outcomeWashoutDays = x$outcomeWashoutDays,
                                                      tar = processTar(
                                                        riskWindowStart = x$riskWindowStart, 
                                                        startAnchor = x$startAnchor,
                                                        riskWindowEnd = x$riskWindowEnd, 
                                                        endAnchor = x$endAnchor
                                                      )))}
          ))),
          covariateSettingId = match(ParallelLogger::convertSettingsToJson(x$covariateSettings),covariateJsonUnique)
        )
        
      }
    )
    ))
  }
  
  # create table
  # add target cohort details
  tempDf <- cohortDefinitionDf
  colnames(tempDf) <- paste0(colnames(tempDf), 'Target')
  rfTargetSettings <- merge(rfTargetSettings, tempDf, by.x = 'targetId', by.y = 'cohortIdTarget')
  rfTargetSettings$setting <- paste0("<a href ='#sec-char-rf-setting-",rfTargetSettings$covariateSettingId,"'>Setting ",rfTargetSettings$covariateSettingId,"</a>")
  rfTargetSettings$outcomeSet <- paste0("<a href ='#sec-char-rf-outcome-",rfTargetSettings$outcomeSet,"'>Outcome ",rfTargetSettings$outcomeSet,"</a>")
  
  rfTargetSettings <- rfTargetSettings %>% 
    dplyr::relocate("cohortNameTarget") %>% 
    dplyr::relocate("parentNameTarget") %>% 
    dplyr::relocate("limitToFirstInNDays", .after = "cohortNameTarget") %>%
    dplyr::relocate("minPriorObservation", .after = "limitToFirstInNDays") %>%
    dplyr::relocate("outcomeSet", .after = "minPriorObservation") %>%
    dplyr::relocate("setting", .after = dplyr::last_col()) %>%
    dplyr::arrange(.data$parentNameTarget, .data$cohortNameTarget)
  
  # add outcome cohort details to each data.frame in list
  tempDf <- cohortDefinitionDf
  colnames(tempDf) <- paste0(colnames(tempDf), 'Outcome')
  rfOutcomeList <- lapply(
    X = rfOutcomeList, 
    FUN = function(outcomedf){
      res <- merge(outcomedf, tempDf, by.x = 'outcomeId', by.y = 'cohortIdOutcome')
      res <- res %>% 
        dplyr::relocate("cohortNameOutcome") %>% 
        dplyr::relocate("parentNameOutcome") %>% 
        dplyr::relocate("outcomeWashoutDays", .after = "cohortNameOutcome") %>%
        dplyr::relocate("tar", .after = "outcomeWashoutDays") %>%
        dplyr::arrange(.data$parentNameOutcome, .data$cohortNameOutcome)
      return(res)
    })
  
  return(
    list(
      targetData = rfTargetSettings,
      outcomeDataList = rfOutcomeList,
      settingsJson = covariateJsonUnique 
    )
  )
  
}



processCaseSeriesSettings <- function(
    CharacterizationModuleSettings,
    cohortDefinitionDf # process cohortDefinition 
    ){
  
  csSpec <- CharacterizationModuleSettings$settings$analysis$caseSeriesSettings
  
  if(is.null(csSpec)){
    return(NULL)
  }
  
  # get the unique covariate settings
  settingsJsonUnique <- unique(lapply(
    X = csSpec, 
    FUN = function(x) ParallelLogger::convertSettingsToJson(
      list(
        caseCovariateSettings = x$caseCovariateSettings, 
        casePreTargetDuration = x$casePreTargetDuration,
        casePostOutcomeDuration = x$casePostOutcomeDuration
      )
    )
  ))
  
  # get unique outcome sets
  csOutcomeList <- unique(lapply(
    X = csSpec, 
    FUN = function(x) {
      data.frame(
        outcomeId = x$outcomeIds,
        outcomeWashoutDays = x$outcomeWashoutDays,
        tar = processTar(
          riskWindowStart = x$riskWindowStart, 
          startAnchor = x$startAnchor,
          riskWindowEnd = x$riskWindowEnd, 
          endAnchor = x$endAnchor
        )
      )
    }))
  
  # get unique target sets
  csTargetList <- unique(lapply(
    X = csSpec, 
    FUN = function(x) {
      data.frame(
        targetId = x$targetIds, # need to account for multiple targetIds
        limitToFirstInNDays = x$limitToFirstInNDays,
        minPriorObservation = x$minPriorObservation
      )
    }
  ))
  
  # extract: target_id, limitToFirstInNDays, minPriorObservation, covariateSettingId
  if(length(csTargetList) == 1){
    # all outcomes by all targets 
    csTargetSettings <- unique(do.call(rbind, lapply(
      X = csSpec, 
      FUN = function(x) {
        
        data.frame(
          targetId = x$targetIds, # need to account for multiple targetIds
          limitToFirstInNDays = x$limitToFirstInNDays,
          minPriorObservation = x$minPriorObservation,
          outcomeSet = 1,
          settingId = match(ParallelLogger::convertSettingsToJson(list(
            caseCovariateSettings = x$caseCovariateSettings, 
            casePreTargetDuration = x$casePreTargetDuration,
            casePostOutcomeDuration = x$casePostOutcomeDuration
          )), settingsJsonUnique)
        )
        
      }
    )
    ))
    
    csOutcomeList <- list(do.call(rbind, csOutcomeList))
    
  } else{
    
    # extract: target_id, limitToFirstInNDays, minPriorObservation, covariateSettingId
    csTargetSettings <- unique(do.call(rbind, lapply(
      X = csSpec, 
      FUN = function(x) {
        
        data.frame(
          targetId = x$targetIds, # need to account for multiple targetIds
          limitToFirstInNDays = x$limitToFirstInNDays,
          minPriorObservation = x$minPriorObservation,
          outcomeSet = which(unlist(lapply(
            X = csOutcomeList,
            FUN = function(y){identical(y, data.frame(outcomeId = x$outcomeIds,
                                                      outcomeWashoutDays = x$outcomeWashoutDays,
                                                      tar = processTar(
                                                        riskWindowStart = x$riskWindowStart, 
                                                        startAnchor = x$startAnchor,
                                                        riskWindowEnd = x$riskWindowEnd, 
                                                        endAnchor = x$endAnchor
                                                      )))}
          ))),
          settingId = match(ParallelLogger::convertSettingsToJson(list(
            caseCovariateSettings = x$caseCovariateSettings, 
            casePreTargetDuration = x$casePreTargetDuration,
            casePostOutcomeDuration = x$casePostOutcomeDuration
          )), settingsJsonUnique)
        )
        
      }
    )
    ))
  }
  
  # create table
  # add target cohort details
  tempDf <- cohortDefinitionDf
  colnames(tempDf) <- paste0(colnames(tempDf), 'Target')
  csTargetSettings <- merge(csTargetSettings, tempDf, by.x = 'targetId', by.y = 'cohortIdTarget')
  csTargetSettings$setting <- paste0("<a href ='#sec-char-cs-setting-",csTargetSettings$settingId,"'>Setting ",csTargetSettings$settingId,"</a>")
  csTargetSettings$outcomeSet <- paste0("<a href ='#sec-char-cs-outcome-",csTargetSettings$outcomeSet,"'>Outcome ",csTargetSettings$outcomeSet,"</a>")
  
  csTargetSettings <- csTargetSettings %>% 
    dplyr::relocate("cohortNameTarget") %>% 
    dplyr::relocate("parentNameTarget") %>% 
    dplyr::relocate("limitToFirstInNDays", .after = "cohortNameTarget") %>%
    dplyr::relocate("minPriorObservation", .after = "limitToFirstInNDays") %>%
    dplyr::relocate("outcomeSet", .after = "minPriorObservation") %>%
    dplyr::relocate("setting", .after = dplyr::last_col()) %>%
    dplyr::arrange(.data$parentNameTarget, .data$cohortNameTarget)
  
  # add outcome cohort details to each data.frame in list
  tempDf <- cohortDefinitionDf
  colnames(tempDf) <- paste0(colnames(tempDf), 'Outcome')
  csOutcomeList <- lapply(
    X = csOutcomeList, 
    FUN = function(outcomedf){
      res <- merge(outcomedf, tempDf, by.x = 'outcomeId', by.y = 'cohortIdOutcome')
      res <- res %>% 
        dplyr::relocate("cohortNameOutcome") %>% 
        dplyr::relocate("parentNameOutcome") %>% 
        dplyr::relocate("outcomeWashoutDays", .after = "cohortNameOutcome") %>%
        dplyr::relocate("tar", .after = "outcomeWashoutDays") %>%
        dplyr::arrange(.data$parentNameOutcome, .data$cohortNameOutcome)
      return(res)
    })
  
  return(
    list(
      targetData = csTargetSettings,
      outcomeDataList = csOutcomeList,
      settingsJson = settingsJsonUnique
    )
  )
  
}


characterizationColDef <- function(){
  res <- list(
    subsetIdTarget = reactable::colDef(show = FALSE),
    subsetIdOutcome = reactable::colDef(show = FALSE),
    isParentTarget = reactable::colDef(show = FALSE),
    isParentOutcome = reactable::colDef(show = FALSE),
    parentIdTarget = reactable::colDef(show = FALSE),
    parentIdOutcome = reactable::colDef(show = FALSE),
    subsetNameTarget = reactable::colDef(show = FALSE),
    subsetNameOutcome = reactable::colDef(show = FALSE),
    packageVersionTarget = reactable::colDef(show = FALSE),
    packageVersionOutcome = reactable::colDef(show = FALSE),
    numberSubsetOperatorsTarget = reactable::colDef(show = FALSE),
    numberSubsetOperatorsOutcome = reactable::colDef(show = FALSE),
    
    subsetId = reactable::colDef(show = FALSE),
    isParent = reactable::colDef(show = FALSE),
    parentId = reactable::colDef(show = FALSE),
    subsetName = reactable::colDef(show = FALSE),
    packageVersion = reactable::colDef(show = FALSE),
    numberSubsetOperators = reactable::colDef(show = FALSE),
    
    cohortIdTarget = reactable::colDef(show = FALSE),
    cohortIdOutcome = reactable::colDef(show = FALSE),
    targetId = reactable::colDef(show = FALSE),
    outcomeId  = reactable::colDef(show = FALSE),
    
    tar = reactable::colDef(
      aggregate = "unique",
      name = 'Time at risk', 
      filterable = TRUE, 
      minWidth = 150
    ),
    outcomeWashoutDays = reactable::colDef(
      aggregate = "unique",
      name = 'Outcome Washout (Days)', 
      filterable = TRUE
    ),
    outcomeSet = reactable::colDef(
      name = 'Outcomes',
      html = TRUE
      ),
    covariateSettingId = reactable::colDef(show = FALSE),
    minPriorObservation = reactable::colDef(
      aggregate = "unique",
      name = 'Prior Obs (days)', 
      filterable = TRUE
    ),
    parentName = reactable::colDef(
      name = 'Parent Cohort', 
      defaultSortOrder = 'asc',
      sortNALast = TRUE,
      filterable = TRUE, 
      minWidth = 300, 
      html = TRUE
    ),
    parentNameTarget = reactable::colDef(
      name = 'Parent Target', 
      defaultSortOrder = 'asc',
      sortNALast = TRUE,
      filterable = TRUE, 
      minWidth = 300,
      html = TRUE
    ),
    parentNameOutcome = reactable::colDef(name = 'Parent Outcome', 
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
    cohortNameOutcome = reactable::colDef(
      show = TRUE,
      aggregate = "unique",
      name = 'Outcome', 
      defaultSortOrder = 'asc',
      sortNALast = TRUE,
      filterable = TRUE, 
      minWidth = 300
    ),
    cohortNameWithLinkOutcome = reactable::colDef(
      show = FALSE,
      name = 'Outcome', 
      defaultSortOrder = 'asc',
      sortNALast = TRUE,
      filterable = TRUE, 
      minWidth = 300,
      html = TRUE
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
    
    cohortName = reactable::colDef(
      show = TRUE,
      aggregate = "unique",
      name = 'Cohort', 
      defaultSortOrder = 'asc',
      sortNALast = TRUE,
      filterable = TRUE, 
      minWidth = 300
    ),
    cohortId = reactable::colDef(show = FALSE),
    
    subsetCohortsTarget = reactable::colDef(
      html = TRUE,
      aggregate = "count"
    ),
    subsetCohorts = reactable::colDef(
      html = TRUE,
      aggregate = "count"
    ),
    subsetCohortsOutcome = reactable::colDef(
      html = TRUE,
      aggregate = "count"
    ),
    
    appliedSubsetsTarget = reactable::colDef(
      html = TRUE,
      aggregate = "count"
    ),
    appliedSubsetsOutcome= reactable::colDef(show = FALSE),
    appliedSubsets = reactable::colDef(
      html = TRUE,
      aggregate = "count"
    ),
    
    settingId = reactable::colDef(show = FALSE),
    setting = reactable::colDef(
      html = TRUE,
      aggregate = "count"
    )
  )
  
  return(res)
}
