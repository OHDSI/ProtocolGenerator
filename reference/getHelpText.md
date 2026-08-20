# getHelpText

For an installed R package, user can specify a function in the package
and in input for the function to get details about the input.

## Usage

``` r
getHelpText(
  package = "CohortMethod",
  functionName = "createPs",
  input = "maxCohortSizeForFitting"
)
```

## Arguments

- package:

  Name of installed R package of interest

- functionName:

  Name of exported function inside R package of interest

- input:

  Name of input into exported function

## Value

Details about the input

## Details

Returns a string with details about the input

## See also

Other Helpers:
[`formatCovariateSettings()`](formatCovariateSettings.md),
[`functionDefaults()`](functionDefaults.md),
[`getAllHelpDetails()`](getAllHelpDetails.md),
[`getAllHelpText()`](getAllHelpText.md),
[`getCohortDefinitionsFromJson()`](getCohortDefinitionsFromJson.md),
[`getConcepts()`](getConcepts.md), [`getDemoLoc()`](getDemoLoc.md),
[`getFunctionFromArgName()`](getFunctionFromArgName.md),
[`getNegativeControlsFromJson()`](getNegativeControlsFromJson.md),
[`getSettingsTable()`](getSettingsTable.md),
[`reportTableFormat()`](reportTableFormat.md),
[`tagPrint()`](tagPrint.md)
