# calcEF3prp

Returns emission factor for manure excreted during pasture range and
paddock. Differs depending on the share of small ruminants.

## Usage

``` r
calcEF3prp(select_years = "y2005")
```

## Arguments

- select_years:

  if only one year is selected, years is set to NULL

## Value

list of magpie object with results on country level, weight on country
level, unit and description.

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("EF3prp")
} # }
```
