# getCiTargetsOutcomes

Extracts a list of target tables, outcome tables, TARs and vector of
unique targetIds and outcomeIds in analysis

## Usage

``` r
getCiTargetsOutcomes(CohortIncidenceModuleSettings, cohortDefinitionDf)
```

## Arguments

- CohortIncidenceModuleSettings:

  The cohort incidence module specification

- cohortDefinitionDf:

  The data.frame with the cohort definition details

## Value

A list with the tables to present in the protocol

## Details

Returns a list

## See also

Other Extraction: [`createStratSentance()`](createStratSentance.md),
[`extractCohortMethodSettings()`](extractCohortMethodSettings.md),
[`getCohortDiagnosticTables()`](getCohortDiagnosticTables.md),
[`getCountStatement()`](getCountStatement.md),
[`getPlpSettings()`](getPlpSettings.md),
[`getSccsSettings()`](getSccsSettings.md)
