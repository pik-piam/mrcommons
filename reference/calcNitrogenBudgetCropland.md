# calcNitrogenBudgetCropland

Calculates Nitrogen Budgets for Cropland soils on country levels.

## Usage

``` r
calcNitrogenBudgetCropland(
  cellular = FALSE,
  deposition = "CEDS",
  include_fertilizer = TRUE,
  max_snupe = 0.85
)
```

## Arguments

- cellular:

  disaggregated to 0.5 degree grid

- deposition:

  if FALSE, deposition is not accounted for in the distribution. Use
  FALSE to avoid circularities in calcNitrogenBudget

- include_fertilizer:

  including fertilizer in budget. Use FALSE to avoid circularities in
  specific cases

- max_snupe:

  NULL or a numeric value. if numeric, an additional N balanceflow is
  included that takes care that the soil nitrogen uptake efficiency does
  not exceed the numeric value in balanceflow.

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("NitrogenBudgetCropland")
} # }
```
