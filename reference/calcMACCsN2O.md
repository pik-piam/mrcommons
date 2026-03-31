# Calculation of N2O MAC curves of Energy Industry and Landuse

Calculation of the N2O relative costcurves (subtypes: Energy Industry
and Landuse) weighted by the baseline emissions. Sources: N2O Transport,
N2O Adipic acid production, N2O Nitric acid production, N2O Fertilizer,
N2O Animal waste, N2O Domestic sewage divided in classes 1-201.

## Usage

``` r
calcMACCsN2O(sector = "all", source = "ImageMacc")
```

## Arguments

- sector:

  "all" or "landuse"; "all"" includes energy_industry and landuse

- source:

  "ImageMacc" or "PBL_MACC_2019"

## Value

MAgPIE object

## See also

[`madrat::calcOutput()`](https://rdrr.io/pkg/madrat/man/calcOutput.html),
[`readImageMacc()`](readImageMacc.md),
[`convertImageMacc()`](convertImageMacc.md)

## Author

Nele Steinmetz, Florian Humpenoeder, Michael Windisch

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("MACCsN2O")
} # }
```
