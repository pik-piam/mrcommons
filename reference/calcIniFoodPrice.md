# calcIniFoodPrice

provides global prices from the IMPACT model projections for MAgPIE
commodities for the initialization of the flexible demand model.

## Usage

``` r
calcIniFoodPrice(datasource = "FAO", year = "y2005", products = "kfo")
```

## Arguments

- datasource:

  The datasource specification. Currently available `FAO` and
  `IMPACT3.2.2World_Price`.

- year:

  Specifies the year for the initialization of prices in MAgPIE. Default
  is `y2005`.

- products:

  subselection of products to be returned

## Value

List with a magpie object with commodity prices on global level in
\$17/tDM.

## Note

The IMPACT projections start in 2005 and the prices are taken from that
year.

## See also

[`readIMPACT3.2.2World_Price()`](readIMPACT3.2.2World_Price.md)

## Author

Mishko Stevanovic, Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("IniFoodPrice", aggregate = FALSE)
} # }
```
