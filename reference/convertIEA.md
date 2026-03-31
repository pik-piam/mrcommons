# Convert IEA

Convert IEA energy data to data on ISO country level.

## Usage

``` r
convertIEA(x, subtype)
```

## Arguments

- x:

  MAgPIE object containing IEA values at IEA mixed country-region
  resolution

- subtype:

  data subtype. Either "EnergyBalances", "EnergyBalances-latest", or
  "Emissions"

## Value

IEA data as MAgPIE object aggregated to country level

## Author

Anastasis Giannousakis, Renato Rodrigues, Falk Benke
