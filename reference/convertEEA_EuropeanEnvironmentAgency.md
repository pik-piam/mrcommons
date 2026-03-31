# Convert European Environment Agency (EEA) data

Read-in European Environment Agency (EEA) data on ETS emissions as
magclass object

## Usage

``` r
convertEEA_EuropeanEnvironmentAgency(x, subtype)
```

## Arguments

- x:

  MAgPIE object to be converted

- subtype:

  data subtype. Either "ETS", "historical", "projections", or
  "projections-detailed"

## Value

magpie object of European Environment Agency (EEA) ETS emissions (GtCO2)

## Author

Renato Rodrigues, Robin Hasse

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource(type = "EEA_EuropeanEnvironmentAgency", subtype = "ETS")
} # }
```
