# calcNitrogenBudgetNonagland

Calculates Nitrogen Budgets for Non-agricultural land.

## Usage

``` r
calcNitrogenBudgetNonagland(
  deposition = "CEDS",
  max_nue = 0.95,
  cellular = FALSE
)
```

## Arguments

- deposition:

  if FALSE, deposition is not accounted for in the distribution. Use
  FALSE to avoid circularities in calcNitrogenBudget

- max_nue:

  NULL or a numeric value. if numeric, an additional N balanceflow is
  included that takes care that the nitrogen use efficiency does not
  exceed the numeric value in balanceflow.

- cellular:

  TRUE returns output on 0.5 degree grid

## Value

Nitrogen Budgets for Non-agricultural land

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("NitrogenBudgetNonagland")
} # }
```
