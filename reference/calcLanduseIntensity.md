# calcLanduseIntensity

This function prepares total tau values for use. As the source data
already provides all required information this function purely removes
unrequired data and moves the xref values to the weighting object which
is required for aggregation.

## Usage

``` r
calcLanduseIntensity(sectoral = "kcr", rescale = TRUE)
```

## Arguments

- sectoral:

  "kcr" (default) for MAgPIE crop items and "lpj" for LPJmL crop items,
  "pasture" for pasture

- rescale:

  TRUE (default), if Tau should be rescaled in a way, that 2010 is
  always 1

## Value

Total tau data and corresponding weights as a list of two MAgPIE objects

## See also

[`madrat::calcTauTotal()`](https://rdrr.io/pkg/madrat/man/calcTauTotal.html),
[`madrat::readTau()`](https://rdrr.io/pkg/madrat/man/readTau.html),
[`madrat::convertTau()`](https://rdrr.io/pkg/madrat/man/convertTau.html)

## Author

Benjamin Leon Bodirsky, Kristine Karstens, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("LanduseIntensity")
} # }
```
