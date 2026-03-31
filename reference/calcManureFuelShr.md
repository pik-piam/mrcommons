# calcManureFuelShr

calculates the share of Manure excreted during grazing which is
collected for fuel. For the future, we assume that with the development,
the fuel share reaches 0.

## Usage

``` r
calcManureFuelShr(products = "magpie")
```

## Arguments

- products:

  IPCC: IPCC products. MAgPIE: Magpie products

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## See also

[`calcExcretion()`](calcExcretion.md)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("ManureFuelShr")
} # }
```
