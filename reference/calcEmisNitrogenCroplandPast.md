# calcEmisNitrogenCroplandPast

calculates nitrogenous emissions from croplands in the historical
period.

## Usage

``` r
calcEmisNitrogenCroplandPast(method = "IPCC")
```

## Arguments

- method:

  IPCC: emissions are calculated according the the IPCC 2006 National
  Guidelines for Greenhouse Gas Inventories. Nsurplus: Emissions in 2005
  are calculated according to IPCC, and the scaled with nitrogen losses
  from croplands.

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("EmisNitrogenCroplandPast")
} # }
```
