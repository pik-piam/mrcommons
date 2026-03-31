# calcFertilizerByCrop

calculates the crop-specific use of different N inputs

## Usage

``` r
calcFertilizerByCrop(
  indicator = "total",
  deposition = "Nsurplus2",
  cellular = FALSE
)
```

## Arguments

- indicator:

  total: estimates the inputs per total crop production; by_harvest
  estimates the inputs per ton harvest; by_area estimates the inputs per
  area harvested

- deposition:

  if FALSE, deposition is not accounted for in the distribution. Use
  FALSE to avoid circularities in calcNitrogenBudget

- cellular:

  cellular disaggreagation or national values

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`calcNitrogenBudgetCropland()`](calcNitrogenBudgetCropland.md)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FertilizerByCrop")
} # }
```
