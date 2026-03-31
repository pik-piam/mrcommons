# Read Eurostat historical emissions (env_air_gge)

Read Eurostat historical emissions (env_air_gge)

## Usage

``` r
convertEurostat(x, subtype)
```

## Arguments

- x:

  MAgPIE object to be converted

- subtype:

  'emissions' for original Eurostat emissions split, 'MACCemi' for MACC
  historical emissions, or 'sectorEmi' for sector specific emissions, or
  'latest' for most up-to-date data

## Value

A MAgPIE object containing the Eurostat historical emissions (MtCO2)

## Author

Renato Rodrigues

## Examples

``` r
if (FALSE) { # \dontrun{
a <- convertEurostat(x, subtype = "emissions")
} # }
```
