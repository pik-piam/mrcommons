# calcNitrogenBudgetPasture

Calculates Nitrogen Budgets for Pasture soils on country levels.

## Usage

``` r
calcNitrogenBudgetPasture(
  cellular = FALSE,
  include_fertilizer = TRUE,
  deposition = "CEDS",
  max_nue = 0.9
)
```

## Arguments

- cellular:

  cellular disaggreagation or national values

- include_fertilizer:

  including fertilizer in budget. Use FALSE to avoid circularities in
  specific cases

- deposition:

  if FALSE, deposition is not accounted for in the distribution. Use
  FALSE to avoid circularities in calcNitrogenBudget

- max_nue:

  NULL or a numeric value. if numeric, an additional N balanceflow is
  included that takes care that the nitrogen use efficiency does not
  exceed the numeric value in balanceflow.

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("NitrogenBudgetPasture")
} # }
```
