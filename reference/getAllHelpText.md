# getAllHelpText

For an installed R package, user can specify a function in the package
and all input details are returned

## Usage

``` r
getAllHelpText(package, functionName)
```

## Arguments

- package:

  Name of installed R package of interest

- functionName:

  Name of exported function inside R package of interest

## Value

Details about all inputs into the functionName within R package of
interest

## Details

Returns a data.frame with the name of all the inputs and descriptions of
the inputs

## See also

Other Helpers:
[`formatCovariateSettings()`](formatCovariateSettings.md),
[`functionDefaults()`](functionDefaults.md),
[`getAllHelpDetails()`](getAllHelpDetails.md),
[`getCohortDefinitionsFromJson()`](getCohortDefinitionsFromJson.md),
[`getConcepts()`](getConcepts.md), [`getDemoLoc()`](getDemoLoc.md),
[`getFunctionFromArgName()`](getFunctionFromArgName.md),
[`getHelpText()`](getHelpText.md),
[`getNegativeControlsFromJson()`](getNegativeControlsFromJson.md),
[`getSettingsTable()`](getSettingsTable.md),
[`reportTableFormat()`](reportTableFormat.md),
[`tagPrint()`](tagPrint.md)
