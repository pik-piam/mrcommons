# calcAWMSconfShrPast

calculates the share of manure excreted in different types of animal
waste management systems in confinements in the year 2005 using the IPCC
Guidelines excretion rates.

## Usage

``` r
calcAWMSconfShrPast(products = "magpie")
```

## Arguments

- products:

  IPCC: IPCC products. MAgPIE: Magpie products

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## See also

[`calcAWMSconfShr()`](calcAWMSconfShr.md),
[`calcExcretionIPCC()`](calcExcretionIPCC.md)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("AWMSconfShrPast")
} # }
```
