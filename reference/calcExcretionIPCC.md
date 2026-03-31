# calcExcretionIPCC

calculates excretion in the year 2005 using the IPCC Guidelines
excretion rates.

## Usage

``` r
calcExcretionIPCC(products = "IPCC")
```

## Arguments

- products:

  IPCC: IPCC products. MAgPIE: Magpie products

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## See also

[`calcExcretion()`](calcExcretion.md),
[`calcAnimalStocks()`](calcAnimalStocks.md)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("ExcretionIPCC")
} # }
```
