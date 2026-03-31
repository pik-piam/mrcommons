# Read in data of FeedModel

Read in csv files containing data on production system distribution and
system-specific feed baskets from the FeedModel "MAgPIE_FEED"

## Usage

``` r
readFeedModel(subtype = "FeedBaskets")
```

## Arguments

- subtype:

  Available subtypes: "ProdSysRatio", "FeedBaskets" and
  "FeedBasketsDetailed"

## Value

magpie object of feed basket data

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Isabelle Weindl

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource(type = "FeedModel", subtype = "FeedBaskets")
} # }
```
