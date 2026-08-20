# getSccsSettings

Extract exposure/outcomes of interest, shared and analysis specific
negative controls, diagnostic settings and analysis settings

## Usage

``` r
getSccsSettings(
  SelfControlledCaseSeriesModuleSettings,
  cohortDefinitionDf,
  negativeControls
)
```

## Arguments

- SelfControlledCaseSeriesModuleSettings:

  The self controlled case series module specification

- cohortDefinitionDf:

  The data.frame with the cohort definition details

- negativeControls:

  The shared negative controls from the json spec

## Value

A list with the tables to display

## Details

Returns a list of tables and settings

## See also

Other Extraction: [`createStratSentance()`](createStratSentance.md),
[`extractCohortMethodSettings()`](extractCohortMethodSettings.md),
[`getCiTargetsOutcomes()`](getCiTargetsOutcomes.md),
[`getCohortDiagnosticTables()`](getCohortDiagnosticTables.md),
[`getCountStatement()`](getCountStatement.md),
[`getPlpSettings()`](getPlpSettings.md)
