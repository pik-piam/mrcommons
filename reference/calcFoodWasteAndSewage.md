# calcFoodWasteAndSewage

Calculates food waste and sewage based on the FAO mass balance or future
projections from Bodirsky et al. (2018).

## Usage

``` r
calcFoodWasteAndSewage(historic = TRUE)
```

## Arguments

- historic:

  If set to TRUE only ho

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`calcFAOmassbalance()`](calcFAOmassbalance.md)

## Author

Benjamin Leon Bodirsky, Xiaoxi Wang

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FoodWasteAndSewage")
} # }
```
