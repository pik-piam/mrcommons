# convertWCDE

It fills the missing countries of the output of readWCDE with the
population-weighted average structure of one or more countries with
similar characteristics, scaled with the population of the missing
country. For years before the population data starts (the historical
reconstruction of subtype "epop_v2" goes back to 1950), the earliest
available population estimate is held constant; this only affects small
countries and territories.

## Usage

``` r
convertWCDE(x, subtype)
```

## Arguments

- x:

  magpie object provided by the read function

- subtype:

  "epop_v3" or "epop_v2" (only needed so that readSource can resolve the
  default subtype consistently for read and convert; the country filling
  is identical for both subtypes)

## See also

[`readWCDE()`](readWCDE.md), [`convertLutz2014()`](convertLutz2014.md)
