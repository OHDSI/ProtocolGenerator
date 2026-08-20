# getCohortDefinitionsFromJson

Extract cohorts from json

## Usage

``` r
getCohortDefinitionsFromJson(json)
```

## Arguments

- json:

  The json analysis specification

## Value

An named R list with the elements subsetUnique (list of subset
operators), cohortDefinitions (list of cohortDefinitions) and
cohortDefinitionDf (data.frame of cohort definitions)

## Details

Returns a names list with the cohorts

## See also

Other Helpers:
[`formatCovariateSettings()`](formatCovariateSettings.md),
[`functionDefaults()`](functionDefaults.md),
[`getAllHelpDetails()`](getAllHelpDetails.md),
[`getAllHelpText()`](getAllHelpText.md),
[`getConcepts()`](getConcepts.md), [`getDemoLoc()`](getDemoLoc.md),
[`getFunctionFromArgName()`](getFunctionFromArgName.md),
[`getHelpText()`](getHelpText.md),
[`getNegativeControlsFromJson()`](getNegativeControlsFromJson.md),
[`getSettingsTable()`](getSettingsTable.md),
[`reportTableFormat()`](reportTableFormat.md),
[`tagPrint()`](tagPrint.md)
