# calcFAOYield

calculates the yield based on FAO data

## Usage

``` r
calcFAOYield(
  physical = TRUE,
  attributes = "dm",
  irrigation = FALSE,
  cellular = FALSE,
  cut = FALSE,
  average = 5,
  areaSource = "FAO"
)
```

## Arguments

- physical:

  physical area or havested area

- attributes:

  in dm, wm, ge, nr, p, k

- irrigation:

  distinguish irrigation or not

- cellular:

  if TRUE value is calculate on cellular level

- cut:

  FALSE (default) - do not cut off yields, number between 0 and 1 to
  define percentile value for cut off

- average:

  averaging period in years (if NULL no averaging is used)

- areaSource:

  data source for croparea used in calculation: FAO or Toolbox

## Value

MAgPIE object of yields

## Author

Debbora Leip, Jan Philipp Dietrich, Kristine Karstens, Felicitas Beier
