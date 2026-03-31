# Read in ImageMacc Costcurves for different subtypes

Read in ImageMacc Costcurves for different subtypes, which are based on
a Paper fro Lucas et al 2007
(http://linkinghub.elsevier.com/retrieve/pii/S1462901106001316) Rows are
removed, the dataframe is reshaped and numbers are replaced by
descriptions.

## Usage

``` r
readImageMacc(subtype)
```

## Arguments

- subtype:

  data subtype. Options are: "CH4_Energy_Industry", "CH4_Landuse",
  "N2O_Energy_Industry", "N2O_Landuse", "HFC_tot", "SF6_tot", "PFC_tot"
  or "baseline_sources"

## Value

magpie object of the ImageMacc data

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
