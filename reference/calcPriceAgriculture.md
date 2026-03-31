# calcPriceAgriculture

provides global prices from the IMPACT model projections, World Bank
observations, and FAO observations for MAgPIE commodities in \$/tDM

## Usage

``` r
calcPriceAgriculture(datasource = "IMPACT3.2.2World_Price", unit = "US$17/tDM")
```

## Arguments

- datasource:

  Options of the source of data: `IMPACT3.2.2World_Price`, `FAO`, `FAOp`
  and `WBGEM`

- unit:

  A string with the unit that should be returned. Options are:

  - "current LCU"

  - "current Int\$PPP"

  - "current US\$MER"

  - "constant YYYY LCU"

  - "constant YYYY Int\$PPP"

  - "constant YYYY US\$MER"

## Value

List with a magpie object with commodity prices

## See also

[`readIMPACT3.2.2World_Price()`](readIMPACT3.2.2World_Price.md),
[`calcWBGEM()`](calcWBGEM.md), [`readWBGEM()`](readWBGEM.md)

## Author

Mishko Stevanovic, Xiaoxi Wang, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("PriceAgriculture", datasource = "IMPACT3.2.2World_Price", aggregate = FALSE)
calcOutput("PriceAgriculture", datasource = "FAO")
calcOutput("PriceAgriculture", datasource = "WBGEM", aggregate = FALSE)
} # }
```
