# calcNitrogenBudgetOcean

Calculates Nitrogen Budgets for Oceans on global level. Values are
placed in Antarcica (ATA)

## Usage

``` r
calcNitrogenBudgetOcean(deposition = "ACCMIP", leaching = "Nsurplus")
```

## Arguments

- deposition:

  Method for calculating Atmospheric deposition: Nsurplus2 and Nsurplus
  are based on deposition rates based on own emission calculations after
  2 or after 1 iteration, respectively.

- leaching:

  Method for calculating leaching: Nsurplus2 and Nsurplus are based on
  deposition rates based on own emission calculations after 2 or after 1
  iteration, respectively.

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("NitrogenBudgetOcean")
} # }
```
