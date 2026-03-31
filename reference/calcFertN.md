# Calculate Fertilizer of N

Provides FertN data for N.No changes to the content have been done.

## Usage

``` r
calcFertN(
  appliedto = "total",
  cellular = FALSE,
  deposition = "CEDS",
  max_snupe = 0.85
)
```

## Arguments

- appliedto:

  'total' (default), 'crop' or 'past'

- cellular:

  cellular disaggreagation or national values

- deposition:

  for disaggregation will be passed on to calcNitrogenBudgetCropland

- max_snupe:

  for disaggregation will be passed on to calcNitrogenBudgetCropland

## Value

Fertilizer data for N and corresonding weights as a list of two MAgPIE
objects

## See also

[`madrat::calcOutput()`](https://rdrr.io/pkg/madrat/man/calcOutput.html),
[`readIFA()`](readIFA.md), [`convertIFA()`](convertIFA.md),
[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Lavinia Baumstark

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FertN")
} # }
```
