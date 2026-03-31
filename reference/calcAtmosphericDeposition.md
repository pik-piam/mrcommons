# calcAtmosphericDeposition

Computes Atmospheric (nitrogen) deposition on different land-use types.
It distinguishes ammonia (Nh3) and Nitrogen oxides (NOx) as well

## Usage

``` r
calcAtmosphericDeposition(
  datasource = "ACCMIP",
  glo_incl_oceans = FALSE,
  cellular = FALSE,
  cells = "lpjcell",
  emission = FALSE,
  scenario = NULL
)
```

## Arguments

- datasource:

  deposition inventory

- glo_incl_oceans:

  provides global values that include oceans, as oceans are not part of
  the country mapping

- cellular:

  cellular or country level emissions

- cells:

  magpiecell (59199 cells) or lpjcell (67420 cells)

- emission:

  if TRUE, not the deposition but the cellular emissions are reported

- scenario:

  if dataset contains several scenarios (e.g. ACCMIP), one scenario can
  be selected.

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`calcNitrogenBudgetCropland()`](calcNitrogenBudgetCropland.md)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("AtmosphericDeposition")
} # }
```
