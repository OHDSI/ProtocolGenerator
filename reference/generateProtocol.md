# generateProtocol

Generates a protocol from a Strategus json specification

## Usage

``` r
generateProtocol(
  jsonLocation,
  json,
  dataDiagnosticFolder = NULL,
  webAPI,
  authMethod = NULL,
  webApiUsername = NULL,
  webApiPassword = NULL,
  outputLocation = getwd(),
  outputName = paste0("protocol_", gsub(":", "_", gsub(" ", "_", as.character(date()))),
    ".html"),
  intermediateDir = tempdir(),
  downloadConcepts = TRUE,
  conceptsAsExcel = FALSE,
  conceptFolder = outputLocation,
  addCohortDefinitions = TRUE,
  exportCohortLocation = NULL,
  headerColor = "#336B91",
  headerLogoLocation = NULL,
  protocolSubheading = NULL
)
```

## Arguments

- jsonLocation:

  The location of the json specification

- json:

  The json specification as an R list (this can be used instead of
  jsonLocation)

- dataDiagnosticFolder:

  The location of the database diagnostic results

- webAPI:

  the webAPI address

- authMethod:

  The webAPI authorization method (optional)

- webApiUsername:

  The webAPI authorization username (optional)

- webApiPassword:

  The webAPI authorization password (optional)

- outputLocation:

  The file location and name to save the protocol

- outputName:

  The name of the html protocol that is created

- intermediateDir:

  The work directory for quarto

- downloadConcepts:

  Whether to download the concepts from webAPI for the cohorts (can be
  slow)

- conceptsAsExcel:

  Whether to save the concepts into excel files rather than embed into
  html file if downloadConcepts is TRUE

- conceptFolder:

  The location to save the excel files if downloadConcepts is TRUE and
  conceptsAsExcel is TRUE

- addCohortDefinitions:

  Whether to add the cohorts to the protocol (can make document large)

- exportCohortLocation:

  if not NULL the location where the table tracter will be exported to
  csv.

- headerColor:

  The CSS color to use for the protocol header banner (optional)

- headerLogoLocation:

  The location of a logo image to add to the protocol header banner
  (optional)

- protocolSubheading:

  Optional subheading to show under the protocol title

## Value

An named R list with the elements 'standard' and 'source'

## Details

Specify the location of the json specification file, the data diagnostic
folder (optional) the ATLAS webAPI (to process cohorts and concepts) and
where you want the protocol saved.
