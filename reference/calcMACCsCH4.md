# Calculation of CH4 MAC curves of Energy Industry and Landuse

Calculation of the CH4 relative costcurves (subtypes: Energy Industry
and Landuse) weighted by the baseline emissions. Sources: CH4 coal
losses/leakages, CH4 oil losses/leakages, CH4 natural gas
losses/leakages, CH4 Landfills, CH4 Domestic Sewage, CH4 Wetland rice,
CH4 Animals, CH4 Animal waste divided in classes 1-201.

## Usage

``` r
calcMACCsCH4(sector = "all", source = "ImageMacc")
```

## Arguments

- sector:

  "all" or "landuse"; "all" includes energy_industry and landuse

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
calcOutput("MACCsCH4")
} # }
```
