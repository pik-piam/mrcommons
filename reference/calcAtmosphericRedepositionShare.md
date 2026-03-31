# calcAtmosphericRedepositionShare

Calculates share of volatilised nitrogen emissions that is redeposited
on different land types.

## Usage

``` r
calcAtmosphericRedepositionShare(
  cellular = FALSE,
  maxshare = 0.8,
  scenario = "rcp45"
)
```

## Arguments

- cellular:

  cellular or country level

- maxshare:

  the maximum amount of emissions deposited within the same cell or
  country. The remainder will be handled as global emission

- scenario:

  scenario

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("AtmosphericRedepositionShare")
} # }
```
