# calcNitrogenWithdrawalByCrop

calculates the crop-specific withdrawals of nutrients from soils

## Usage

``` r
calcNitrogenWithdrawalByCrop(
  indicator = "total",
  cellular = FALSE,
  irrigation = FALSE
)
```

## Arguments

- indicator:

  total: estimates the inputs per total crop production; by_area
  estimates the inputs per area harvested

- cellular:

  cellular disaggreagation or national values

- irrigation:

  FALSE for the sum of irrigated and rainfed, FALSE for separated
  categories, 'rainfed' or 'irrigated for single categories

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
calcOutput("NitrogenWithdrawalByCrop")
} # }
```
