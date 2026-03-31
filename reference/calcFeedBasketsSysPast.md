# Calculate historical system-specific feed baskets based on output of MAgPIE_FEED model as DM feed biomass (different types of feed) needed per DM livestock products of respective systems

Calculate historical system-specific feed baskets based on output of
MAgPIE_FEED model as DM feed biomass (different types of feed) needed
per DM livestock products of respective systems

## Usage

``` r
calcFeedBasketsSysPast(faoVersion = "join2010", yearly = FALSE)
```

## Arguments

- faoVersion:

  which version of FAO food baskets to use

- yearly:

  whether to calculate yearly data or only magpie 5year timesteps

## Value

Historical system-specific feed baskets and corresponding weights as a
list of two MAgPIE objects

## See also

[`madrat::calcOutput()`](https://rdrr.io/pkg/madrat/man/calcOutput.html),
[`readFeedModel()`](readFeedModel.md),
[`calcFeedBasketsPast()`](calcFeedBasketsPast.md)

## Author

Isabelle Weindl, Benjamin Bodirsky, Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FeedBasketsSysPast")
} # }
```
