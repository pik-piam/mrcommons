# calcFeedPast

Combines feed baskets of the past with livestock production to get total
feed demand

## Usage

``` r
calcFeedPast(
  balanceflow = TRUE,
  cellular = FALSE,
  cells = "lpjcell",
  products = "kall",
  nutrients = "all",
  yearly = FALSE
)
```

## Arguments

- balanceflow:

  if TRUE, a feed balance flow is included in total feed demand, if not
  it is excluded.

- cellular:

  if TRUE value is calculated on cellular level with returned data just
  in dry matter

- cells:

  Switch between "magpiecell" (59199) and "lpjcell" (67420)

- products:

  products in feed baskets that shall be reported

- nutrients:

  nutrients like dry matter (DM), reactive nitrogen (Nr), Phosphorus
  (P), Generalizable Energy (GE) and wet matter (WM).

- yearly:

  whether to calculate yearly data or only magpie 5year timesteps

## Value

List of magpie objects with results on country or cellular level, unit
and description.

## Author

Isabelle Weindl, Benjamin Leon Bodirsky, Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FeedPast")
} # }
```
