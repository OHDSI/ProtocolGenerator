# reportTableFormat

create a grouped reactable::reactable

## Usage

``` r
reportTableFormat(
  table,
  groupBy = NULL,
  columns = NULL,
  elementId = NULL,
  caption,
  groupByButton = FALSE
)
```

## Arguments

- table:

  data.frame or tibble with the data to present

- groupBy:

  column to group by (optional)

- columns:

  The column details (create default using defaultColumns())

- elementId:

  Element ID for the widget.

- caption:

  A table caption

- groupByButton:

  Whether to add a button that lets you group/ungroup rows in the table

## Value

Details about all inputs into the functionName within R package of
interest

## Details

Returns a reactable::reactable

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
[`getSettingsTable()`](getSettingsTable.md), [`tagPrint()`](tagPrint.md)
