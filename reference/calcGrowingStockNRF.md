# calcGrowingStockNRF

Calculates the growing stocks from FAO data for naturally regenerating
forests i.e. primary forests and secondary forests.

## Usage

``` r
calcGrowingStockNRF(indicator = "per_ha")
```

## Arguments

- indicator:

  Either "per_ha" (m3/ha, area-weighted) or "total" (Mm3, absolute).

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## Author

Abhijeet Mishra, Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("GrowingStockNRF", indicator = "per_ha", aggregate = TRUE)
calcOutput("GrowingStockNRF", indicator = "total", aggregate = TRUE)
} # }
```
