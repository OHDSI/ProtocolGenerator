# getConcepts

Extract concept id details for a vector of concept ids

## Usage

``` r
getConcepts(
  expression = NULL,
  conceptIds = NULL,
  baseUrl = "https://api.ohdsi.org/WebAPI"
)
```

## Arguments

- expression:

  A concept set expression to extract details about from ATLAS webapi

- conceptIds:

  A vector of conceptsIds to extract details about from ATLAS webapi

- baseUrl:

  The ATLAS baseUrl

## Value

An named R list with the elements 'standard' and 'source'

## Details

Returns a names list of length two with 'standard' and 'source'
containing a data.frame with the concept ids details for the standard
concepts and their sourced concepts.

## See also

Other Helpers:
[`formatCovariateSettings()`](formatCovariateSettings.md),
[`functionDefaults()`](functionDefaults.md),
[`getAllHelpDetails()`](getAllHelpDetails.md),
[`getAllHelpText()`](getAllHelpText.md),
[`getCohortDefinitionsFromJson()`](getCohortDefinitionsFromJson.md),
[`getDemoLoc()`](getDemoLoc.md),
[`getFunctionFromArgName()`](getFunctionFromArgName.md),
[`getHelpText()`](getHelpText.md),
[`getNegativeControlsFromJson()`](getNegativeControlsFromJson.md),
[`getSettingsTable()`](getSettingsTable.md),
[`reportTableFormat()`](reportTableFormat.md),
[`tagPrint()`](tagPrint.md)
