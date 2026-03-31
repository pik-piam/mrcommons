# calcFeedBasketsPast

Calculate historical feed baskets based on output of MAgPIE_FEED model
as DM feed biomass (different types of feed) needed per DM livestock
products

## Usage

``` r
calcFeedBasketsPast(
  non_eaten_food = TRUE,
  faoVersion = "join2010",
  yearly = FALSE
)
```

## Arguments

- non_eaten_food:

  if TRUE, non-eaten food is included in feed baskets, if not it is
  excluded.

- faoVersion:

  which version of FAO food baskets to use

- yearly:

  whether to calculate yearly data or only magpie 5year timesteps

## Value

Historical feed baskets and corresponding weights as a list of two
MAgPIE objects

## See also

[`madrat::calcOutput()`](https://rdrr.io/pkg/madrat/man/calcOutput.html),
[`readFeedModel()`](readFeedModel.md)

## Author

Isabelle Weindl, Benjamin Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FeedBasketsPast")
} # }
```
