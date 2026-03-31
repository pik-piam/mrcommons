# calcFAOmassbalance

Calculates a massbalance dataset of agricultural production, processing
and use out of the combined data of calcFAOharmonized(). Covers dry
matter (DM), reactive nitrogen (Nr), Phosphorus (P), Generalizable
Energy (GE) and wet matter (WM). New products are added to the Food
Balance Sheets, and many processing conversions are made more explicit
using simple assumptions. The first part of this function is the
calcFAOmassbalance_pre.

## Usage

``` r
calcFAOmassbalance(version = "join2010", yearly = FALSE)
```

## Arguments

- version:

  whether to use the pre ("pre2010") or post ("post2010") 2010 versions
  of FAOSTAT Food balances, or "join2010" which joins them at 2010

- yearly:

  whether to calculate yearly data or only magpie 5year timesteps

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`mrfaocore::calcFAOmassbalance_pre()`](https://rdrr.io/pkg/mrfaocore/man/calcFAOmassbalance_pre.html)

## Author

Benjamin Leon Bodirsky, Xiaoxi Wang, David Chen

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FAOmassbalance")
} # }
```
