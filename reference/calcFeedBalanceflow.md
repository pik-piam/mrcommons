# calcFeedBalanceflow

Calculates feed balance flows from MAgPIE-Feed model to meet FAO data

## Usage

``` r
calcFeedBalanceflow(
  per_livestock_unit = FALSE,
  cellular = FALSE,
  cells = "lpjcell",
  products = "kall",
  future = "constant",
  yearly = FALSE
)
```

## Arguments

- per_livestock_unit:

  default false

- cellular:

  if TRUE value is calculated on cellular level

- cells:

  Switch between "magpiecell" (59199) and "lpjcell" (67420)

- products:

  products in feed baskets that shall be reported

- future:

  if FALSE, only past years will be reported (reduces memory)

- yearly:

  whether to calculate yearly data or only magpie 5year timesteps

## Value

List of magpie objects with results on country or cellular level, unit
and description.

## Author

Isabelle Weindl, Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FeedBalanceflow")
} # }
```
