# calcProduction

Distributes crop, pasture and livestock production in space to 0.5
degree

## Usage

``` r
calcProduction(
  products = "kcr",
  cellular = FALSE,
  cells = "lpjcell",
  calibrated = TRUE,
  attributes = "all",
  irrigation = FALSE
)
```

## Arguments

- products:

  setname of products ("kcr", "kli", "pasture")

- cellular:

  if TRUE production is calculate on cellular level

- cells:

  Switch between "magpiecell" (59199) and "lpjcell" (67420)

- calibrated:

  if FALSE, lpj yields will be used uncalibrated, if true, calibrated on
  FAP production on country level

- attributes:

  "All" for all crop attributes, or specify e.g. DM (dry matter), Nr
  (nitrogen) for memory reduction

- irrigation:

  if TRUE, additional information on irrigated and rainfed production is
  provided

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`mrlandcore::calcLanduseInitialisation()`](https://rdrr.io/pkg/mrlandcore/man/calcLanduseInitialisation.html),
[`mrlandcore::calcCroparea()`](https://rdrr.io/pkg/mrlandcore/man/calcCroparea.html)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("Production")
} # }
```
