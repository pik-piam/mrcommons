# readSoilGrids

This function reads the raw SoilGrids data (available at
https://files.isric.org/soilgrids/data/recent/OCSTHA_M_30cm_250m_ll.tif)
or if available the preprocessed raster layers.

## Usage

``` r
readSoilGrids(subtype)
```

## Arguments

- subtype:

  Switch between different input. Use predefined ones or any FileName
  specified in 'SoilGrids/META_GEOTIFF_1B.csv'

## Value

magpie object in cellular resolution

## See also

[`downloadSoilGrids()`](downloadSoilGrids.md)

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("SoilGrids", subtype = "cstock_0_30")
} # }
```
