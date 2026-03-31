# calcFeedBaskets

Combines feed baskets of the past with scenario-dependent future feed
baskets.

## Usage

``` r
calcFeedBaskets(non_eaten_food = FALSE, fadeout = FALSE, method = "new")
```

## Arguments

- non_eaten_food:

  if TRUE, non-eaten food is included in feed baskets, if not it is
  excluded.

- fadeout:

  if TRUE, feed basket calibration fades out till 2050.

- method:

  "new" for additive calibration at end, "old" for multiplicative
  calibration of calShr and end values.

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## Author

Isabelle Weindl, Benjamin Leon Bodirsky, Stephen Wirth, Jan Philipp
Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FeedBaskets")
} # }
```
