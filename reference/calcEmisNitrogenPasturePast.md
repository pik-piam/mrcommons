# calcEmisNitrogenPasturePast

Calculates nitrogenous emissions from pastures for the historical period

## Usage

``` r
calcEmisNitrogenPasturePast(method = "IPCC")
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

## See also

[`calcEmisNitrogenPast()`](calcEmisNitrogenPast.md),
[`calcExcretion()`](calcExcretion.md)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("EmisNitrogenPasturePast")
} # }
```
