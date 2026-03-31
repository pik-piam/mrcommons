# calcResDemand

Calculates the demand for Crop Residues

## Usage

``` r
calcResDemand(cellular = FALSE, yearly = FALSE)
```

## Arguments

- cellular:

  If TRUE calculation and output on cellular level

- yearly:

  whether to calculate yearly data or only magpie 5 year time steps

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`calcResBiomass()`](calcResBiomass.md)

## Author

Benjamin Leon Bodirsky, Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("ResDemand")
} # }
```
