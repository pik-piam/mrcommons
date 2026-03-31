# convertICP2017

converts data of World Bank ICP round, downloaded from here:
https://databank.worldbank.org/source/icp-2017 mainly a currency
conversion to MER2017. Assume that in the original dataset, 2011 values
are in 2011MER and 2017 in 2017MER, given the ICP rounds

## Usage

``` r
convertICP2017(x, subtype = "per_cap_expMER")
```

## Arguments

- x:

  MAgPIE object containing original values

- subtype:

  data subtype needed. Either "priceLevel", or "per_cap_exp"

## Value

magpie object of relative price levels (world = 100) or per capita
expenditure (USD17 MER)

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
a <- convertSource("ICP2017", "per_cap_exp")
} # }
```
