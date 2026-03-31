# calcSNUpE

calculates the soil nitrogen uptake efficiency. This is the nitrogen
taken up from the soil (N in crop biomass minus biological fixation
minus seed N) divided by the soil N inputs (fertilizer, manure etc). For
the future, SNuPE scenarios are added.

## Usage

``` r
calcSNUpE(max_snupe = 0.85, cellular = FALSE, rev = 0.1, maccbase = TRUE)
```

## Arguments

- max_snupe:

  Maximum realistic SNUPE. All values above will be limited to this
  value. Only holds for past values; future scneario values can exceed
  this number.

- cellular:

  disaggregated to 0.5 degree grid

- rev:

  revision number of madrat run

- maccbase:

  whether future scenarios should be expressed as base efficiency,
  excluding additional macc improvemetns (new default)

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`calcNitrogenBudgetCropland()`](calcNitrogenBudgetCropland.md)

## Author

Benjamin Leon Bodirsky, Xiaoxi Wang

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("SNUpE")
} # }
```
