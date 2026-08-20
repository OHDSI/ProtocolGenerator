# extractCohortMethodSettings

Extract cohorts from json

## Usage

``` r
extractCohortMethodSettings(
  cohortMethodModuleSettings,
  negativeControls,
  cohortDefinitionDf
)
```

## Arguments

- cohortMethodModuleSettings:

  The cohort method module specification

- negativeControls:

  NULL or a data.frame of the negative controls

- cohortDefinitionDf:

  The data.frame with the cohort definition details

## Value

An named R list with ...

## Details

Returns a names list with the cohorts

## See also

Other Extraction: [`createStratSentance()`](createStratSentance.md),
[`getCiTargetsOutcomes()`](getCiTargetsOutcomes.md),
[`getCohortDiagnosticTables()`](getCohortDiagnosticTables.md),
[`getCountStatement()`](getCountStatement.md),
[`getPlpSettings()`](getPlpSettings.md),
[`getSccsSettings()`](getSccsSettings.md)
