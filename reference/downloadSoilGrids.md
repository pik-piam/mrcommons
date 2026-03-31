# downloadSoilGrids

This function downloads the raw SoilGrids data (available at
https://files.isric.org/soilgrids/data/recent) or, if available, the
preprocessed raster layers.

## Usage

``` r
downloadSoilGrids(subtype = "cstock_0_30")
```

## Arguments

- subtype:

  Switch between different input. Use predefined ones or any FileName
  specified in 'SoilGrids/META_GEOTIFF_1B.csv'

## Value

magpie object in cellular resolution

## See also

[`readSoilGrids()`](readSoilGrids.md)

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
downloadSource("SoilGrids", subtype = "carbon0_30")
} # }
```
