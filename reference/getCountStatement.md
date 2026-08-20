# getCountStatement

Create a sentence that explains the number of targets, outcomes and
settings per analysis.

## Usage

``` r
getCountStatement(CohortIncidenceModuleSettings, cohortDefinitionDf)
```

## Arguments

- CohortIncidenceModuleSettings:

  The cohort incidence module specification

- cohortDefinitionDf:

  The data.frame with the cohort definition details

## Value

An string with the count information

## Details

Returns a string

## See also

Other Extraction: [`createStratSentance()`](createStratSentance.md),
[`extractCohortMethodSettings()`](extractCohortMethodSettings.md),
[`getCiTargetsOutcomes()`](getCiTargetsOutcomes.md),
[`getCohortDiagnosticTables()`](getCohortDiagnosticTables.md),
[`getPlpSettings()`](getPlpSettings.md),
[`getSccsSettings()`](getSccsSettings.md)
