# cmColDef

Extract cohorts from json

## Usage

``` r
cmColDef(elementId = "cm-tc-tab", colNames = NULL)
```

## Arguments

- elementId:

  An element id for the table using this column definitions (needed for
  the drop down selection)

- colNames:

  Optional a vector of column names to restrict to

## Value

A column definition list

## Details

Returns a names list with the cohorts

## See also

Other ColDefs: [`cmOutcomeColDef()`](cmOutcomeColDef.md),
[`defaultColumns()`](defaultColumns.md),
[`getCIcolumns()`](getCIcolumns.md), [`getCdCols()`](getCdCols.md),
[`getPlpColDefs()`](getPlpColDefs.md),
[`getSccsColDefs()`](getSccsColDefs.md)
