# calcLandEmissions

Land emission data. This function was originally called
calcValidEmissions and located in mrvalidation.

## Usage

``` r
calcLandEmissions(datasource = "CEDS")
```

## Arguments

- datasource:

  The Emission Inventory that shall be used. For futher information,
  best see function calcEmissionInventory. Options are e.g. CEDS,
  combined_CEDS_IPCC (including own estimates where available), IPCC(own
  estimates), Nsurplus (own estimates)

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## Author

Benjamin Leon Bodirsky, Michael S. Crawford

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("LandEmissions")
} # }
```
