# calcEmisNitrogenCroplandPast

providees an emission inventory for the past, either from external data
or own estimates.

## Usage

``` r
calcEmissionInventory(
  datasource = "CEDS",
  targetResolution = "sectoral",
  from = "CEDS59",
  to = "Sectors"
)
```

## Arguments

- datasource:

  The Inventory that shall be used. Options are CEDS, combined_CEDS_IPCC
  (including own estimates where available), IPCC(own estimates),
  Nsurplus (own estimates)

- targetResolution:

  Specific mapping file to be used.

- from:

  column in mapping

- to:

  column in mapping

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## Author

Benjamin Leon Bodirsky, Michael S. Crawford

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("EmisNitrogenCroplandPast")
} # }
```
