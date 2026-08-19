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
  
  # add new variables if they are not NULL
  tSize <- CharacterizationModuleSettings$settings$minTargetSize
  cSize <- CharacterizationModuleSettings$settings$minCaseSize
  
  # Updates for Char v4 inputs
  if(!is.null(tSize)){
    txt <- paste0(txt, ' Only run risk factor/case series for study populations >= ', tSize, ' people.')
  }
  if(!is.null(tSize)){
    txt <- paste0(txt, ' Only run risk factor/case series for cases (number of people with outcome) >= ', cSize, ' people.')
  }
  
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
  
  # only non-NULL in v4 or higher
  characterizationTargetLookup <- CharacterizationModuleSettings$settings$analysis$characterizationTargetLookup
  
  if(is.null(tbSpec)){
    return(NULL)
  }
    

  # helper at bottom of file
  targetPop <- getTargetPop(
    spec = tbSpec, 
    settingName = 'targetBaselineSettings',
    characterizationTargetLookup = characterizationTargetLookup,
    mapCovariates = TRUE, 
    mapOutcomes = FALSE
  )
  
  targetSettings <-  targetPop$targetSettings
  covariateJsonUnique  <- targetPop$covariateJsonUnique
  
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
  # only non-NULL in v4 or higher
  characterizationTargetLookup <- CharacterizationModuleSettings$settings$analysis$characterizationTargetLookup
  
  
  if(is.null(rfSpec)){
    return(NULL)
  }
  
  # check whether the same targets are used in all settings
  allTs <- getTargetPop(
    spec = rfSpec, 
    characterizationTargetLookup = characterizationTargetLookup, 
    settingName = 'riskFactorSettings',
    mapCovariates = FALSE,
    mapOutcomes = FALSE
  )$targetSettings
  row.names(allTs) <- NULL
  
  firstTs <-  getTargetPop(
    spec = list(rfSpec[[1]]), 
    characterizationTargetLookup = characterizationTargetLookup, 
    settingName = 'riskFactorSettings',
    mapCovariates = FALSE,
    mapOutcomes = FALSE
  )$targetSettings
  row.names(firstTs) <- NULL
  
  singleTargetSet <- all.equal(allTs, firstTs)
  
  if(singleTargetSet){
    # all outcomes by all targets 
    targetPop <- getTargetPop(
        spec = rfSpec, 
        settingName = 'riskFactorSettings',
        characterizationTargetLookup = characterizationTargetLookup,
        mapCovariates = TRUE, 
        mapOutcomes = TRUE
      )
    
    rfTargetSettings <-  unique(targetPop$targetSettings)
    rfTargetSettings$outcomeSet <- 1
    rfTargetSettings <-  unique(rfTargetSettings)
    covariateJsonUnique  <- targetPop$covariateJsonUnique
    rfOutcomeList <- targetPop$outcomeList
  
    rfOutcomeList <- list(unique(do.call(rbind, rfOutcomeList)))
    
  } else{
    
    # extract: target_id, limitToFirstInNDays, minPriorObservation, covariateSettingId
    targetPop <- getTargetPop(
      spec = rfSpec, 
      settingName = 'riskFactorSettings',
      characterizationTargetLookup = characterizationTargetLookup,
      mapCovariates = TRUE, 
      mapOutcomes = TRUE
    )
    rfTargetSettings <- targetPop$targetSettings
    covariateJsonUnique  <- targetPop$covariateJsonUnique
    rfOutcomeList <- targetPop$outcomeList
    
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
  characterizationTargetLookup <- CharacterizationModuleSettings$settings$analysis$characterizationTargetLookup
  
  if(is.null(csSpec)){
    return(NULL)
  }
  
  # check whether the same targets are used in all settings
  allTs <- getTargetPop(
    spec = csSpec, 
    characterizationTargetLookup = characterizationTargetLookup, 
    settingName = 'caseSeriesSettings',
    mapCovariates = FALSE,
    mapOutcomes = FALSE
  )$targetSettings
  row.names(allTs) <- NULL
  
  firstTs <-  getTargetPop(
    spec = list(csSpec[[1]]), 
    characterizationTargetLookup = characterizationTargetLookup, 
    settingName = 'caseSeriesSettings',
    mapCovariates = FALSE,
    mapOutcomes = FALSE
  )$targetSettings
  row.names(firstTs) <- NULL
  
  singleTargetSet <- all.equal(allTs, firstTs)
  
  if(singleTargetSet){
    # all outcomes by all targets 
    targetPop <- getTargetPop(
      spec = csSpec, 
      settingName = 'caseSeriesSettings',
      characterizationTargetLookup = characterizationTargetLookup,
      mapCovariates = FALSE, 
      mapOutcomes = TRUE,
      mapCaseSeries = TRUE
    )
    
    csTargetSettings <-  unique(targetPop$targetSettings)
    csTargetSettings$outcomeSet <- 1
    csTargetSettings <-  unique(csTargetSettings)
    settingsJsonUnique  <- targetPop$caseJsonUnique
    csOutcomeList <- targetPop$outcomeList
    
    csOutcomeList <- list(unique(do.call(rbind, csOutcomeList)))
    
  } else{
    
    # extract: target_id, limitToFirstInNDays, minPriorObservation, covariateSettingId
    targetPop <- getTargetPop(
      spec = csSpec, 
      settingName = 'caseSeriesSettings',
      characterizationTargetLookup = characterizationTargetLookup,
      mapCovariates = FALSE, 
      mapOutcomes = TRUE,
      mapCaseSeries = TRUE
    )
    csTargetSettings <- targetPop$targetSettings
    settingsJsonUnique  <- targetPop$caseJsonUnique
    csOutcomeList <- targetPop$outcomeList
    
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



# Time-to-event
processTimeToEventSettings <- function(
    CharacterizationModuleSettings,
    cohortDefinitionDf # process cohortDefinition 
){
  
  tteSpec <- CharacterizationModuleSettings$settings$analysis$timeToEventSettings
  
  # only non-NULL in v4 or higher
  characterizationTargetLookup <- CharacterizationModuleSettings$settings$analysis$characterizationTargetLookup
  
  if(is.null(tteSpec)){
    return(NULL)
  }
  
  # work for earlier versions and v4
  if(is.null(characterizationTargetLookup)){
    
    popList <- lapply(
      X = tteSpec, 
      FUN = function(x){
        cohortDefinitionDf[cohortDefinitionDf$cohortId %in% x$targetIds, ]
        }
    )
    
  } else{
    
    popList <- lapply(
      X = tteSpec, 
      FUN = function(x){
        merge(
          cohortDefinitionDf,
          characterizationTargetLookup %>%
            dplyr::filter(.data$timeToEventSettings == 1) %>%
            dplyr::select(!dplyr::any_of(c("timeToEventSettings", "dechallengeRechallengeSettings", "targetBaselineSettings", "riskFactorSettings", "caseSeriesSettings"))),
          by.x = 'cohortId', 
          by.y = 'targetId'
        )
      }
    )
    
  }
  
  outcomeList <- lapply(
    X = tteSpec, 
    FUN = function(x){cohortDefinitionDf[cohortDefinitionDf$cohortId %in% x$outcomeIds,]}
  )
  
  return(
    list(
      popList = popList,
      outcomeList = outcomeList
    )
  )
  
}

#=====================
# Dechal-rechal
#=====================
processDechalSettings <- function(
    CharacterizationModuleSettings,
    cohortDefinitionDf # process cohortDefinition 
){
  
  dcSpec <- CharacterizationModuleSettings$settings$analysis$dechallengeRechallengeSettings
  
  # only non-NULL in v4 or higher
  characterizationTargetLookup <- CharacterizationModuleSettings$settings$analysis$characterizationTargetLookup
  
  if(is.null(dcSpec)){
    return(NULL)
  }
  
  settingsList <- lapply(
    X = dcSpec, 
    FUN = function(x){list(
      dechallengeStopInterval = x$dechallengeStopInterval,
      dechallengeEvaluationWindow = x$dechallengeEvaluationWindow
      )}
  )
  
  # work for earlier versions and v4
  if(is.null(characterizationTargetLookup)){
    
    popList <- lapply(
      X = dcSpec, 
      FUN = function(x){
        cohortDefinitionDf[cohortDefinitionDf$cohortId %in% x$targetCohortDefinitionIds, ]
      }
    )
    
  } else{
    
    popList <- lapply(
      X = dcSpec, 
      FUN = function(x){
        merge(
          cohortDefinitionDf,
          characterizationTargetLookup %>%
            dplyr::filter(.data$dechallengeRechallengeSettings == 1) %>%
            dplyr::select(!dplyr::any_of(c("timeToEventSettings", "dechallengeRechallengeSettings", "targetBaselineSettings", "riskFactorSettings", "caseSeriesSettings"))),
          by.x = 'cohortId', 
          by.y = 'targetId'
        )
      }
    )
    
  }
  
  outcomeList <- lapply(
    X = dcSpec, 
    FUN = function(x){cohortDefinitionDf[cohortDefinitionDf$cohortId %in% c(x$outcomeIds,x$outcomeCohortDefinitionIds),]}
  )
  
  return(
    list(
      popList = popList,
      outcomeList = outcomeList,
      settingsList = settingsList
    )
  )
  
}

#=====================
# Col def helpers
#=====================
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





# helper 
getTargetPop <- function(
    spec, 
    characterizationTargetLookup, 
    settingName = 'targetBaselineSettings',
    mapCovariates = FALSE,
    mapOutcomes = FALSE,
    mapCaseSeries = FALSE
    
    ){
  
  if(mapCovariates){
    # get the unique covariate settings
    covariateJson <- lapply(
      X = spec, 
      FUN = function(x) ParallelLogger::convertSettingsToJson(x$covariateSettings)
    )
    covariateJsonUnique <- unique(covariateJson)
  } else{
    covariateJsonUnique <- NULL
  }
  
  if(mapOutcomes){
    
    outcomeList <- unique(lapply(
      X = spec, 
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
    
  } else{
    outcomeList = NULL
  }
  
  
  if(mapCaseSeries){
    caseJsonUnique<- unique(lapply(
      X = spec, 
      FUN = function(x) ParallelLogger::convertSettingsToJson(
        list(
          caseCovariateSettings = x$caseCovariateSettings, 
          casePreTargetDuration = x$casePreTargetDuration,
          casePostOutcomeDuration = x$casePostOutcomeDuration
        )
      )
    ))
  } else{
    caseJsonUnique <- NULL
  }
  
if(is.null(spec[[1]]$characterizationTargetIds)){
  # Spec prior to v4 char
  # extract: target_id, limitToFirstInNDays, minPriorObservation, covariateSettingId
  targetSettings <- do.call(rbind, lapply(
    X = spec, 
    FUN = function(x){
      temp <- data.frame(
        targetId = x$targetIds, 
        limitToFirstInNDays = x$limitToFirstInNDays,
        minPriorObservation = x$minPriorObservation
      )
    if(mapCovariates){
      temp$covariateSettingId = match(ParallelLogger::convertSettingsToJson(x$covariateSettings),covariateJsonUnique)
    }
    
    if(mapOutcomes){
      temp$outcomeSet = which(unlist(lapply(
        X = outcomeList,
        FUN = function(y){identical(y, data.frame(outcomeId = x$outcomeIds,
                                                  outcomeWashoutDays = x$outcomeWashoutDays,
                                                  tar = processTar(
                                                    riskWindowStart = x$riskWindowStart, 
                                                    startAnchor = x$startAnchor,
                                                    riskWindowEnd = x$riskWindowEnd, 
                                                    endAnchor = x$endAnchor
                                                  )))}
      )))
    }  
      
      if(mapCaseSeries){
        temp$settingId = match(ParallelLogger::convertSettingsToJson(
          list(
            caseCovariateSettings = x$caseCovariateSettings, 
            casePreTargetDuration = x$casePreTargetDuration,
            casePostOutcomeDuration = x$casePostOutcomeDuration
          )
        ),caseJsonUnique)
      }
      
      return(temp)
    }
  )
  )} else{
    
    # v4 char
    targetSettings <- do.call(rbind, lapply(
      X = spec, 
      FUN = function(x){
        temp <- data.frame(
          characterizationTargetId = x$characterizationTargetIds
        )
        if(mapCovariates){
          temp$covariateSettingId = match(ParallelLogger::convertSettingsToJson(x$covariateSettings),covariateJsonUnique)
        }
        if(mapOutcomes){
          temp$outcomeSet = which(unlist(lapply(
            X = outcomeList,
            FUN = function(y){identical(y, data.frame(outcomeId = x$outcomeIds,
                                                      outcomeWashoutDays = x$outcomeWashoutDays,
                                                      tar = processTar(
                                                        riskWindowStart = x$riskWindowStart, 
                                                        startAnchor = x$startAnchor,
                                                        riskWindowEnd = x$riskWindowEnd, 
                                                        endAnchor = x$endAnchor
                                                      )))}
          )))
        } 
        
        if(mapCaseSeries){
          temp$settingId = match(ParallelLogger::convertSettingsToJson(
            list(
              caseCovariateSettings = x$caseCovariateSettings, 
              casePreTargetDuration = x$casePreTargetDuration,
              casePostOutcomeDuration = x$casePostOutcomeDuration
            )
          ),caseJsonUnique)
        }
        
        return(temp)
      }
    ))
    
    
    # need to join only studyPops
    targetSettings <- merge(targetSettings, characterizationTargetLookup, by = 'characterizationTargetId')
    
    targetSettings$settings <- targetSettings[,settingName]
    
    # filter to targetBaselineSettings == 1 and remove timeToEventSettings dechallengeRechallengeSettings targetBaselineSettings riskFactorSettings caseSeriesSettings
    targetSettings <- targetSettings %>%
      dplyr::filter(.data$settings == 1) %>%
      dplyr::select(!dplyr::any_of(c("settings","timeToEventSettings", "dechallengeRechallengeSettings", "targetBaselineSettings", "riskFactorSettings", "caseSeriesSettings")))
    
  }
  
  return(
    list(
      targetSettings = unique(targetSettings),
      covariateJsonUnique = covariateJsonUnique,
      outcomeList = outcomeList,
      caseJsonUnique = caseJsonUnique
    )
  )
}
