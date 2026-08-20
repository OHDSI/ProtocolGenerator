# getSettingsTable

This returns a tibble with the input details for the function of
interest in the package of interest plus add the user setting values for
the inputs adds a column called 'bold' that is TRUE if the user setting
value is different to the default

## Usage

``` r
getSettingsTable(package, functionName, settings)
```

## Arguments

- package:

  Name of installed R package of interest

- functionName:

  Name of exported function inside R package of interest

- settings:

  ...

## Value

Returns a tibble with the input details

## Details

This returns a tibble with the input details

## See also

Other Helpers:
[`formatCovariateSettings()`](formatCovariateSettings.md),
[`functionDefaults()`](functionDefaults.md),
[`getAllHelpDetails()`](getAllHelpDetails.md),
[`getAllHelpText()`](getAllHelpText.md),
[`getCohortDefinitionsFromJson()`](getCohortDefinitionsFromJson.md),
[`getConcepts()`](getConcepts.md), [`getDemoLoc()`](getDemoLoc.md),
[`getFunctionFromArgName()`](getFunctionFromArgName.md),
[`getHelpText()`](getHelpText.md),
[`getNegativeControlsFromJson()`](getNegativeControlsFromJson.md),
[`reportTableFormat()`](reportTableFormat.md),
[`tagPrint()`](tagPrint.md)
