# calcNuePasture

calculates the soil nitrogen uptake efficiency of pastures. \# This is
the nitrogen taken up from the soil (N in crop biomass minus biological
fixation minus seed N) divided by the soil N inputs (fertilizer, manure
etc). For the future, NUE scenarios are added.

## Usage

``` r
calcNuePasture(cellular = FALSE, maccbase = TRUE)
```

## Arguments

- cellular:

  cellular disaggreagation or national values

- maccbase:

  whether future scenarios should be expressed as base efficiency,
  excluding additional macc improvements (new default)

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`calcSNUpE()`](calcSNUpE.md)
[`calcNitrogenBudgetPasture()`](calcNitrogenBudgetPasture.md)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("NuePasture")
} # }
```
