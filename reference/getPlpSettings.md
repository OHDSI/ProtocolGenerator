# getPlpSettings

Extract plp tables and settings from json

## Usage

``` r
getPlpSettings(PatientLevelPredictionModuleSettings, cohortDefinitionDf)
```

## Arguments

- PatientLevelPredictionModuleSettings:

  The patient level prediction module specification

- cohortDefinitionDf:

  The data.frame with the cohort definition details

## Value

An named R list with ...

## Details

Returns a names list with the tables and settings

## See also

Other Extraction: [`createStratSentance()`](createStratSentance.md),
[`extractCohortMethodSettings()`](extractCohortMethodSettings.md),
[`getCiTargetsOutcomes()`](getCiTargetsOutcomes.md),
[`getCohortDiagnosticTables()`](getCohortDiagnosticTables.md),
[`getCountStatement()`](getCountStatement.md),
[`getSccsSettings()`](getSccsSettings.md)
