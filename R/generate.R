#' getDemoLoc
#'
#' @description
#' Get the file location of an example json in the package
#'
#' @details
#' This function returns the path to an example json specification file.
#' 
#' @return
#' A file path location to an example specification json
#'
#' @export
#' 
getDemoLoc <- function(){
  return(system.file(
    'demoJson', 
    "example.json", 
    package = "ProtocolGenerator"
  ))
}

#' generateProtocol
#'
#' @description
#' Generates a protocol from a Strategus json specification
#'
#' @details
#' Specify the location of the json specification file, the data diagnostic folder (optional)
#' the ATLAS webAPI (to process cohorts and concepts) and where you want the
#' protocol saved.
#' 
#' @param jsonLocation The location of the json specification
#' @param json The json specification as an R list (this can be used instead of jsonLocation)
#' @param dataDiagnosticFolder The location of the database diagnostic results
#' @param webAPI the webAPI address
#' @param authMethod The webAPI authorization method (optional) 
#' @param webApiUsername The webAPI authorization username (optional) 
#' @param webApiPassword The webAPI authorization password (optional) 
#' @param outputLocation The file location and name to save the protocol 
#' @param outputName The name of the html protocol that is created
#' @param intermediateDir The work directory for quarto
#' @param downloadConcepts Whether to download the concepts from webAPI for the cohorts (can be slow)
#' @param conceptsAsExcel Whether to save the concepts into excel files rather than embed into html file if downloadConcepts is TRUE
#' @param conceptFolder The location to save the excel files if downloadConcepts is TRUE and conceptsAsExcel is TRUE
#' @param addCohortDefinitions Whether to add the cohorts to the protocol (can make document large)
#' @param exportCohortLocation if not NULL the location where the table tracter will be exported to csv.
#' 
#' @return
#' An named R list with the elements 'standard' and 'source'
#'
#' @export
#' 
generateProtocol <- function(
    jsonLocation,
    json,
    dataDiagnosticFolder = NULL,
    webAPI,
    authMethod = NULL,
    webApiUsername = NULL,
    webApiPassword = NULL,
    outputLocation = getwd(),
    outputName = paste0('protocol_', gsub(':', '_',gsub(' ','_',as.character(date()))),'.html'),
    intermediateDir = tempdir(),
    downloadConcepts = TRUE,
    conceptsAsExcel = FALSE,
    conceptFolder = outputLocation,
    addCohortDefinitions = TRUE,
    exportCohortLocation = NULL
){
  
  if(missing(jsonLocation)){
    if(missing(json)){
      stop('Must enter either jsonLocation or json')
    } else{
      
      # could have issues if multiple runs at the same time?
      jsonLocation <- file.path(tempdir(), 'spec.json')
      
      ParallelLogger::saveSettingsToJson(
        object = json, 
        fileName = jsonLocation
        )
    }
  }
  
  protocolLoc <- system.file(
    'protocol', 
    package = "ProtocolGenerator"
    )
  
  if(!dir.exists(file.path(intermediateDir, 'protocol'))){
    dir.create(file.path(intermediateDir, 'protocol'), recursive = T)
  }
  if(!dir.exists(file.path(intermediateDir, 'protocol', 'prediction'))){
    dir.create(file.path(intermediateDir, 'protocol', 'prediction'), recursive = T)
  }
  
  filesOfInt <- c(
    dir(protocolLoc, pattern = '.Rmd'),
    dir(protocolLoc, pattern = '.qmd')
    )
  
  file.copy(
    from = file.path(protocolLoc, filesOfInt), 
    to = file.path(file.path(intermediateDir, 'protocol'), filesOfInt)
    )
  
  file.copy(
    from = file.path(
      protocolLoc, 
      'prediction',
      dir(file.path(protocolLoc, 'prediction' )) 
      ), 
    to = file.path(
      intermediateDir, 
      'protocol', 
      'prediction', 
      dir(file.path(protocolLoc, 'prediction' ))
      )
    )

  quarto::quarto_render(
    input = file.path(intermediateDir, 'protocol', "assure_study_protocol.qmd"), 
    #output_file = outputName,
    execute_params = list(
      jsonLocation = jsonLocation,
      dataDiagnosticFolder = dataDiagnosticFolder,
      webAPI = webAPI,
      authMethod = authMethod,
      webApiUsername = webApiUsername,
      webApiPassword = webApiPassword,
      downloadConcepts = downloadConcepts,
      addCohortDefinitions = addCohortDefinitions,
      conceptsAsExcel = conceptsAsExcel,
      conceptFolder = conceptFolder,
      exportCohortLocation = exportCohortLocation
    )
  )
  
  # now move html output to output location
  if(!dir.exists(outputLocation)){
    dir.create(outputLocation, recursive = T)
  }
  file.copy(
    from = file.path(intermediateDir, 'protocol', 'assure_study_protocol.html'), 
    to = file.path(outputLocation, outputName)
      )
  
  return(file.path(outputLocation, outputName))
}