# calcTimberTradeBilateral

A very rough disaggregation of timber demand to bilateral trade flows

## Usage

``` r
calcTimberTradeBilateral(products = "magpie")
```

## Arguments

- products:

  if "magpie" do UNIT (m3 –\> MT) and name conversion of the 2 magpie
  wood products, else "FAO" gives original ones

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`mrfaocore::calcFAOmassbalance_pre()`](https://rdrr.io/pkg/mrfaocore/man/calcFAOmassbalance_pre.html)

## Author

David M Chen

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("TimberTradeBilateral")
} # }
```
