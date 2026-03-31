# Read European Environment Agency (EEA) data

Read-in European Environment Agency (EEA) data on ETS emissions as
magclass object

## Usage

``` r
readEEA_EuropeanEnvironmentAgency(subtype)
```

## Arguments

- subtype:

  data subtype. Either "ETS", "ESR", "total", "sectoral", "projections",
  or "projections-detailed"

## Value

magpie object of European Environment Agency (EEA) ETS emissions (GtCO2)

## Author

Renato Rodrigues, Falk Benke, Robin Hasse

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource(type = "EEA_EuropeanEnvironmentAgency", subtype = "ETS")
} # }
```
