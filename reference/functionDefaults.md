# functionDefaults

For an installed R package, user can specify a function in the package
and all default input values returned

## Usage

``` r
functionDefaults(package, functionName)
```

## Arguments

- package:

  Name of installed R package of interest

- functionName:

  Name of exported function inside R package of interest

## Value

Details about all default inputs into the functionName within R package
of interest

## Details

Returns a named list with the name of all the inputs and the default
input values

## See also

Other Helpers:
[`formatCovariateSettings()`](formatCovariateSettings.md),
[`getAllHelpDetails()`](getAllHelpDetails.md),
[`getAllHelpText()`](getAllHelpText.md),
[`getCohortDefinitionsFromJson()`](getCohortDefinitionsFromJson.md),
[`getConcepts()`](getConcepts.md), [`getDemoLoc()`](getDemoLoc.md),
[`getFunctionFromArgName()`](getFunctionFromArgName.md),
[`getHelpText()`](getHelpText.md),
[`getNegativeControlsFromJson()`](getNegativeControlsFromJson.md),
[`getSettingsTable()`](getSettingsTable.md),
[`reportTableFormat()`](reportTableFormat.md),
[`tagPrint()`](tagPrint.md)
