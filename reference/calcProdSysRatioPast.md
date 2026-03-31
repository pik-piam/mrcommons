# Calculate historical distribution of livestock production across different systems based on output of MAgPIE_FEED model

Calculate historical distribution of livestock production across
different systems based on output of MAgPIE_FEED model

## Usage

``` r
calcProdSysRatioPast(faoVersion = "join2010", yearly = FALSE)
```

## Arguments

- faoVersion:

  which version of FAO food balances to use in mass balance

- yearly:

  whether to calculate yearly data or only magpie 5year timesteps

## Value

Historical distribution of livestock production across different systems
and corresponding weights as a list of two MAgPIE objects

## See also

[`madrat::calcOutput()`](https://rdrr.io/pkg/madrat/man/calcOutput.html),
[`readFeedModel()`](readFeedModel.md)

## Author

Isabelle Weindl

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("ProdSysRatioPast")
} # }
```
