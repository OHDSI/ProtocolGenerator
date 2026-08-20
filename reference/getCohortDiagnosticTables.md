# getCohortDiagnosticTables

Extract target, setting and feature table from CohortDiagnosticsSettings

## Usage

``` r
getCohortDiagnosticTables(CohortDiagnosticsSettings, cohortDefinitionDf)
```

## Arguments

- CohortDiagnosticsSettings:

  The cohort diagnostic module specification

- cohortDefinitionDf:

  The data.frame with the cohort definition details

## Value

A list with the tables to display

## Details

Returns a list of tables

## See also

Other Extraction: [`createStratSentance()`](createStratSentance.md),
[`extractCohortMethodSettings()`](extractCohortMethodSettings.md),
[`getCiTargetsOutcomes()`](getCiTargetsOutcomes.md),
[`getCountStatement()`](getCountStatement.md),
[`getPlpSettings()`](getPlpSettings.md),
[`getSccsSettings()`](getSccsSettings.md)
