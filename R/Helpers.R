# Copyright 2023 Observational Health Data Sciences and Informatics
#
# This file is part of ProtocolGenerator
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


#' display a reactable when there are multiple outputs to print
#'
#' @description
#' display a reactable when there are multiple outputs to print
#'
#' @details
#' Wraps the input around print and shiny:tagList
#' 
#' @param x The object, such as a reactable, to print 
#' @return
#' Nothing just prints the object in quarto
#'
#' @export
#' 
tagPrint <- function(x){
  print(shiny::tagList(x))
}

#' getCohortDefinitionsFromJson
#'
#' @description
#' Extract cohorts from json
#'
#' @details
#' Returns a names list with the cohorts
#' 
#' @param json The json analysis specification
#' @return
#' An named R list with the elements subSetDefs, cohortIds, cohortNames and cohortDefinitions
#'
#' @export
#' 
getCohortDefinitionsFromJson <- function(
    json
){
  
  cohortDefinitions <- json$sharedResources[[which("cohortDefinitions" == unlist(lapply(json$sharedResources, function(x) names(x))))]]$cohortDefinitions
  
  cohortNames <- as.data.frame(do.call('rbind', cohortDefinitions))
  
  # append subsets to cohortDefinitions
  
  subsetDefInd <- which(unlist(lapply(json$sharedResources, function(x) "subsetDefs" %in% names(x))))
  subsetDefs <- json$sharedResources[[subsetDefInd]]$subsetDefs
  
  subSetDefsNice <- data.frame(
    subsetName = unlist(
      lapply(1:length(subsetDefs), function(i){
        paste0(jsonlite::fromJSON(subsetDefs[i])$subsetOperators$name, collapse = ' - ')
      })
    ),
    subsetId = unlist(
      lapply(1:length(subsetDefs), function(i){
        jsonlite::fromJSON(subsetDefs[i])$definitionId
      })
    ),
    json = subsetDefs
  )
  
  # now get the actual subsets
  cohortSubsetsInd <- which(unlist(lapply(json$sharedResources, function(x) "cohortSubsets" %in% names(x))))
  cohortSubsets <- json$sharedResources[[cohortSubsetsInd]]$cohortSubsets
  cohortSubsets <- as.data.frame(do.call('rbind', cohortSubsets))
  
  cohortSubsets <- merge(cohortSubsets, subSetDefsNice, by = 'subsetId', sort = F)
  cohortSubsets <- merge(
    cohortSubsets, 
    cohortNames, 
    by.x = 'targetCohortId', 
    by.y = 'cohortId',
    sort = F)
  
  for(i in 1:ncol(cohortSubsets)){
    cohortSubsets[,i] <- unlist(cohortSubsets[,i])
  }
  cohortSubsets$cohortName <- paste0(cohortSubsets$cohortName, '-', cohortSubsets$subsetName)
  #cohortSubsets$cohortDefinition <- paste0(cohortSubsets$cohortDefinition, '-' ,cohortSubsets$json)
  cohortSubsets$subsetDefinition <- cohortSubsets$json

  cohortSubsetsDefinitions <- lapply(
    1:nrow(cohortSubsets), 
    function(i){
      list(
        cohortId = cohortSubsets$cohortId[i],
        cohortName = cohortSubsets$cohortName[i],
        cohortDefinition = cohortSubsets$cohortDefinition[i],
        subsetDefinition = cohortSubsets$subsetDefinition[i]
        )
    }
      )
  
  cohortDefinitions <- append(cohortDefinitions, cohortSubsetsDefinitions)
    
  cohortIds <- unlist(lapply(cohortDefinitions, function(x) x$cohortId))
  cohortNames <- unlist(lapply(cohortDefinitions, function(x) x$cohortName))
  
  # return subSetDefsNice (add to appendix), plus cohortIds, cohortNames and cohortDefinitions
  
  return(list(
    subSetDefs = subSetDefsNice,
    cohortIds = cohortIds,
    cohortNames = cohortNames,
    cohortDefinitions = cohortDefinitions
  ))
}


#' getConcepts
#'
#' @description
#' Extract concept id details for a vector of concept ids
#'
#' @details
#' Returns a names list of length two with 'standard' and 'source' containing 
#' a data.frame with the concept ids details for the standard concepts and their
#' sourced concepts.
#' 
#' @param expression A concept set expression to extract details about from ATLAS webapi
#' @param conceptIds A vector of conceptsIds to extract details about from ATLAS webapi
#' @param baseUrl The ATLAS baseUrl
#' @return
#' An named R list with the elements 'standard' and 'source'
#'
#' @export
#' 
getConcepts <- function(
    expression, 
    conceptIds = NULL,
    baseUrl = 'https://api.ohdsi.org/WebAPI'
    ){
  
  # if concepts are not specified, extract from the expression instead
  if(is.null(conceptIds)){
    allCodes <- ROhdsiWebApi::resolveConceptSet(
      conceptSetDefinition = expression, 
      baseUrl = baseUrl
    )
  } else{
    allCodes <- conceptIds
  }
  
  standard <- ROhdsiWebApi::getConcepts(
    conceptIds = allCodes, 
    baseUrl = baseUrl
  )
  
  source <- ROhdsiWebApi::getSourceConcepts(
    conceptIds = allCodes, 
    baseUrl = baseUrl
  )
  
  return(list(
    standard = standard,
    source = source
  ))
  
}



#' getFunctionFromArgName
#'
#' @description
#' This gets the input name from the arg name as CohortMethod ass Arg to the inputs
#'
#' @details
#' Returns the name of the input the setting arg corresponds to
#' 
#' @param package Name of installed R package of interest
#' @param argumentName Name of the input
#' @return
#' the name of the input the setting arg corresponds to
#'
#' @export
#' 
getFunctionFromArgName <- function(
    package, 
    argumentName
    ){
  
  argumentName <- gsub('Args','', argumentName)
  packageFunctions <- gsub('.Rd','',names(tools::Rd_db(package)))
  result <- packageFunctions[unlist(lapply(packageFunctions, function(x) length(grep(argumentName, x))>0))]
  
  return(result)
}

#' getHelpText
#'
#' @description
#' For an installed R package, user can specify a function in the package
#' and in input for the function to get details about the input.
#'
#' @details
#' Returns a string with details about the input
#' 
#' @param package Name of installed R package of interest
#' @param functionName Name of exported function inside R package of interest
#' @param input  Name of input into exported function
#' @return
#' Details about the input
#'
#' @export
#' 
getHelpText <- function(
    package = 'CohortMethod',
    functionName = 'createPs',
    input = 'maxCohortSizeForFitting'
){
  
  helpText <- tools::Rd_db(package)
  names(helpText) <- gsub('.Rd','',names(helpText))
  if(functionName %in% names(helpText)){
    textOfInt <- helpText[functionName]
    textOfInt <- paste0(as.character(textOfInt[[1]]), collapse = '')
  } else {
    return("")
  }
  
  
  if(is.null(input)){
    
    val <- paste0(
      "^.*description\\{\\s*|\\s*",
      #"\\}.*$"
      "\n\\}.*$"
    )
    
    desc <- gsub(val, '', textOfInt)
    return(desc)
  }
  
  val <- paste0(
    "^.*item\\{",input,"\\}\\{\\s*|\\s*",
    #"\\}.*$"
    "\\}\n\n.*$"
  )
  
  desc <- gsub(val, '', textOfInt)
  return(desc)
}

#' getAllHelpText
#'
#' @description
#' For an installed R package, user can specify a function in the package
#' and all input details are returned
#'
#' @details
#' Returns a data.frame with the name of all the inputs and descriptions of
#' the inputs
#' 
#' @param package Name of installed R package of interest
#' @param functionName Name of exported function inside R package of interest
#' @return
#' Details about all inputs into the functionName within R package of interest
#'
#' @export
#' 
getAllHelpText <- function(
    package, 
    functionName
    ){
  inputs <- formals(eval(parse(text = paste0(package,'::', functionName))))
  helperDetails <- data.frame(
    input = names(inputs),
    desc = unlist(
      lapply(names(inputs), function(x){getHelpText(input = x, package = package, functionName = functionName)})
    )
  )
  return(helperDetails)
}


getExtraCyclopsHelp <- function(){
  
  # <TODO> add code to ensure Cyclops is installed!
  
  controlInputs <- formals(eval(parse(text = 'Cyclops::createControl')))
  controlHelperDetails <- data.frame(
    input = names(controlInputs),
    desc = unlist(
      lapply(names(controlInputs), function(x){getHelpText(input = x, package = 'Cyclops', functionName = 'createControl')})
    )
  )
  
  priorInputs <- formals(eval(parse(text = 'Cyclops::createPrior')))
  priorHelperDetails <- data.frame(
    input = names(priorInputs),
    desc = unlist(
      lapply(names(priorInputs), function(x){getHelpText(input = x, package = 'Cyclops', functionName = 'createPrior')})
    )
  )
  
  return(
    list(
      control = controlHelperDetails,
      prior = priorHelperDetails
    )
  )
  
}

#' functionDefaults
#'
#' @description
#' For an installed R package, user can specify a function in the package
#' and all default input values returned
#'
#' @details
#' Returns a named list with the name of all the inputs and the default input
#' values
#' 
#' @param package Name of installed R package of interest
#' @param functionName Name of exported function inside R package of interest
#' @return
#' Details about all default inputs into the functionName within R package of interest
#'
#' @export
#' 
functionDefaults <- function(
    package, 
    functionName
    ){
  inputs <- formals(eval(parse(text = paste0(package,'::', functionName))))
  
  # CohortMethod uses Cyclops function without referencing it
  # so need to add this code to address that
  if(package == "CohortMethod"){
    if("Cyclops" %in% rownames(utils::installed.packages())){
      createPrior <- Cyclops::createPrior
      createControl <- Cyclops::createControl
    } else{
      createPrior <- function(x){'need to install Cyclops'}
      createControl <- function(x){'need to install Cyclops'}
    }
  }
  
  if(!is.null(inputs)){
    hasValue <- unlist(
      lapply(inputs, function(x){
        !inherits(x, 'name')
      })
    )
    
    defaultInputs <-  lapply(1:length(inputs), 
                             function(i){
                               if(hasValue[i]){
                                 if(!is.null(
                                   tryCatch(
                                     {eval(inputs[[i]])},
                                     error = function(e){paste0(as.character(inputs[[i]]), collapse = ' ')}
                                   )
                                 )){
                                   return(
                                     tryCatch(
                                       {eval(inputs[[i]], envir = inputs)}, # checking this works
                                       error = function(e){paste0(as.character(inputs[[i]]), collapse = ' ')}
                                       )
                                   )
                                 } else{
                                   return("NULL")
                                 }
                               } else{
                                 return("no default")
                               }
                             }
    )
    names(defaultInputs) <- names(hasValue)
    return(defaultInputs)
  }
}

# settings to data.frame
# TODO - user recursion here?
listToDf <- function(
    settings, 
    valueName = 'value'
    ){
  values <- unlist(settings)
  names <- names(values)
  
  df <- data.frame(
    input = unlist(lapply(strsplit(names, '\\.'), function(x) x[1])),
    level2 = unlist(lapply(strsplit(names, '\\.'), function(x) ifelse(is.na(x[2]), " ", x[2]))),
    value = values, 
    row.names = NULL
  ) 
  
  colnames(df)[colnames(df) == "value"] <- valueName
  
  return(df)
}


#' getAllHelpDetails
#'
#' @description
#' For an installed R package, user can specify a function in the package
#' and all default input values plus input descriptions are returned
#'
#' @details
#' Returns a data.frmae with the name of all the inputs, the default input
#' values and a description about the inputs
#' 
#' @param package Name of installed R package of interest
#' @param functionName Name of exported function inside R package of interest
#' @return
#' Details about all inputs into the functionName within R package of interest
#'
#' @export
#' 
getAllHelpDetails <- function(
    package, 
    functionName
    ){
  
  # get the main function text
  mainText <- getHelpText(
    package = package, 
    functionName = functionName,
    input = NULL
  )
  
  # get default values and convert into a data.frame
  defaults <- functionDefaults(
    package = package, 
    functionName = functionName
    )
  defaultsDf <- listToDf(
    defaults, 
    valueName = 'defaultValue'
    )
  
  # get all the inputs desc:
  description <- getAllHelpText(
    package = package, 
    functionName = functionName
    )
  description$level2 <- ' '
  
  # hack to add Cyclops stuff - dirty code
  if('control' %in% names(defaults)){
    cyclopsHelp <- getExtraCyclopsHelp()
    
    description <- rbind(
      description,
      cyclopsHelp$control %>% 
        dplyr::rename(level2 = "input") %>%
        dplyr::mutate(input = 'control') %>%
        dplyr::relocate("input")
    )
  }
  
  # hack to add Cyclops stuff - dirty code
  if('prior' %in% names(defaults)){
    cyclopsHelp <- getExtraCyclopsHelp()
    
    description <- rbind(
      description,
      cyclopsHelp$prior %>% 
        dplyr::rename(level2 = "input") %>%
        dplyr::mutate(input = 'prior') %>%
        dplyr::relocate("input")
    )
  }
  
  # merge defaults and descriptions
  descAndDefault <- merge(
    defaultsDf, 
    description, 
    by = c('input', 'level2')
    )
  
  return(descAndDefault)
}


#' getSettingsTable
#'
#' @description
#' This returns a tibble with the input details for the function of interest
#' in the package of interest plus add the user setting values for the inputs
#' adds a column called 'bold' that is TRUE if the user setting value is different
#' to the default
#'
#' @details
#' This returns a tibble with the input details
#' 
#' @param package Name of installed R package of interest
#' @param functionName Name of exported function inside R package of interest
#' @param settings  ...
#' @return
#' Returns a tibble with the input details
#'
#' @export
#' 
getSettingsTable <- function(
    package, 
    functionName,
    settings
){
  
  descAndDefault <- getAllHelpDetails(
    package, 
    functionName
    )
  
  settingsDf <- listToDf(
    settings, 
    valueName = 'value'
    )
  
  completeTb <- merge(
    descAndDefault, 
    settingsDf, 
    by = c('input', 'level2'),
    all.y = T
  )
  
  # check whether defaultValue = value
  completeTb <- completeTb %>%
    dplyr::mutate(bold = .data$value != .data$defaultValue) %>%
    dplyr::select(-"defaultValue")
  
  return(completeTb)
}


#' defaultColumns
#'
#' @description
#' create reactable colunn definition where
#' if the column bold is TRUE or NA the column
#' value had red font otherwise black
#' the bold column is then hidden and column level2
#' had no name shown
#'
#' @details
#' Returns a reactable colunn definition
#' 
#' @param data the data.frame to use in reactable
#' @return
#' Returns a reactable colunn definition
#'
#' @export
#' 
defaultColumns <- function(data){
  return(
    list(
      value = reactable::colDef(
        style = function(value, index) {
          if (is.na(data$bold[index])) {
            font_weight <- "red"
          } else if(data$bold[index]) {
            font_weight <- "red"
          } else {
            font_weight <- "black"
          }
          list(color = font_weight)
        }
      ),
      level2 = reactable::colDef(
        name = ""
      ),
      bold = reactable::colDef(
        show = F
      )
    )
  )
}


#' reportTableFormat
#'
#' @description
#' create a grouped reactable::reactable
#'
#' @details
#' Returns a reactable::reactable
#' 
#' @param table data.frame or tibble with the data to present
#' @param groupBy column to group by (optional)
#' @param columns The column details (create default using defaultColumns())
#' @param caption  A table caption
#' @param elementId Element ID for the widget.
#' @return
#' Details about all inputs into the functionName within R package of interest
#'
#' @export
#' 
reportTableFormat <- function(
    table, 
    groupBy = NULL,
    columns = NULL, 
    elementId = NULL,
    caption
){
  
  reactable::reactable(
    data = table,
    groupBy = groupBy, 
    striped = T, 
    searchable = T,
    resizable = T, 
    defaultPageSize = 5,
    showPageSizeOptions = T,
    showSortIcon = T, 
    columns = columns, 
    rownames = F,
    elementId = elementId
  )
  
}

#' formatCovariateSettings
#'
#' @description
#' changes the covariateSettings list to a nice table format
#'
#' @details
#' Returns a data.frame with the covariate settings
#' 
#' @param covariateSettings  The covariate settings 
#' @return
#' a data.frame with the covariate settings
#'
#' @export
#' 
formatCovariateSettings <- function(
    covariateSettings
    ){
  if(inherits(covariateSettings, 'covariateSettings')){
    covariateSettings <- list(covariateSettings)
  }
  
  covariateDetails <- c()
  for(j in 1:length(covariateSettings)){
    tempSettings <- covariateSettings[[j]]
    # TODO - make flexible - for now assume covariates all come from FE
    if(attr(tempSettings,"fun") == "getDbDefaultCovariateData"){
      fun <- 'createCovariateSettings'
      package <- 'FeatureExtraction'
      # add use to certain inputs
      
      inds <- c(
        grep('Demographics', names(tempSettings)),
        grep('Drug', names(tempSettings)),
        grep('Visit', names(tempSettings)),
        grep('Condition', names(tempSettings)),
        grep('Procedure', names(tempSettings)),
        grep('Measurement', names(tempSettings)),
        grep('Observation', names(tempSettings))
      )
      names(tempSettings)[unique(inds)] <- paste0('use',names(tempSettings)[unique(inds)])
    } else if(attr(tempSettings,"fun") == "PatientLevelPrediction::getCohortCovariateData"){
      fun <- 'createCohortCovariateSettings'
      package <- 'PatientLevelPrediction'
    } else if(attr(tempSettings,"fun") == "getDbCohortBasedCovariatesData"){
      fun <- 'createCohortBasedCovariateSettings'
      package <- 'FeatureExtraction'
    } else{
      fun <- attr(tempSettings,"fun")
      package <- ''
    }
    
    covariateDetailsTemp <- getSettingsTable(
      package = package, 
      functionName = fun, 
      settings = tempSettings
    ) %>%
      dplyr::mutate(level2 = .data$input) %>%
      dplyr::mutate(input = paste0('Covariate ', j))
    
    covariateDetails <- rbind(
      covariateDetails,
      covariateDetailsTemp
    )
  }
  
  return(covariateDetails)
}




