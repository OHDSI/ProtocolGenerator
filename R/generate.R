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
    'demo', 
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
#' @param dataDiagnosticFolder The location of the database diagnostic results
#' @param webAPI the webAPI address
#' @param outputLocation The file location and name to save the protocol 
#' @param outputName The name of the html protocol that is created
#' @param intermediateDir The work directory for quarto
#' @return
#' An named R list with the elements 'standard' and 'source'
#'
#' @export
#' 
generateProtocol <- function(
    jsonLocation = getDemoLoc(),
    dataDiagnosticFolder = NULL,
    webAPI,
    outputLocation = getwd(),
    outputName = paste0('protocol_', gsub(':', '_',gsub(' ','_',as.character(date()))),'.html'),
    intermediateDir = tempdir()
){
  
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
      webAPI = webAPI
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