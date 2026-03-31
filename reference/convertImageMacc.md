# Convert subtypes of the ImageMacc data

Convert subtypes from ImageMacc to data on ISO country level. Correct
values for N2O of the subtype "baseline_sources" from N to N2O (factor:
44/28).

## Usage

``` r
convertImageMacc(x, subtype)
```

## Arguments

- x:

  MAgPIE object containing ImageMacc data mixed on region level

- subtype:

  data subtype. Either CH4_Energy_Industry", "CH4_Landuse",
  "N2O_Energy_Industry", "N2O_Landuse", "HFC_tot", "SF6_tot", "PFC_tot"
  or "baseline_sources"

## Value

ImageMacc data as MAgPIE object for all subtypes aggregated to country
level

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Nele Steinmetz

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("ImageMacc", "CH4_Energy_Industry")
a <- readSource("ImageMacc", "CH4_Landuse")
a <- readSource("ImageMacc", "N2O_Energy_Industry")
a <- readSource("ImageMacc", "N2O_Landuse")
a <- readSource("ImageMacc", "HFC_tot")
a <- readSource("ImageMacc", "SF6_tot")
a <- readSource("ImageMacc", "PFC_tot")
a <- readSource("ImageMacc", "baseline_sources")
} # }
```
