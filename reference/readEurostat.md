# Read Eurostat historical emissions (env_air_gge)

Read-in Eurostat historical emissions csv files as magclass object

## Usage

``` r
readEurostat(subtype = "emissions")
```

## Arguments

- subtype:

  'emissions' for original Eurostat emissions split, 'MACCemi' for MACC
  historical emissions, or 'sectorEmi' for sector specific emissions, or
  'latest' for most up-to-date data

## Value

magpie object of Eurostat historical emissions (MtCO2)

## Author

Renato Rodrigues

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource(type = "Eurostat", subtype = "emissions")
} # }
```
