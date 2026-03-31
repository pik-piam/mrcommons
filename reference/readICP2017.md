# readICP2017

Reads data of World Bank ICP round, downloaded from here:
https://databank.worldbank.org/source/icp-2017

## Usage

``` r
readICP2017(subtype = "per_cap_expMER")
```

## Arguments

- subtype:

  data subtype to be read in. Available subtypes are:

  - `priceLevel` Price level index (World = 100)

  - `expRatio` Expenditure component share of GDP (GDP = 100%)

  - `exp_LCU` Expenditure (local currency units, billions)

  - `exp_MER` Expenditure, market exchange rate-based (US\$, billions)

  - `exp_PPP` Expenditure, PPP-based (US\$, billions)

  - `per_cap_expPPP` Expenditure per capita, PPP-based (US\$)

  - `per_cap_expMER` Expenditure per capita, market exchange rate-based
    (US\$)

## Value

magpie object of relative price levels (world = 100) or per capita
expenditure (USD17 MER)

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("ICP2017", "per_cap_exp")
} # }
```
