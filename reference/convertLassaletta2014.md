# convertLassaletta2014

converts the dataset of Lassaletta, L., G. Billen, B. Grizzetti, J.
Angalde, and J. Garnier. 2014. 50 Year Trends in Nitrogen Use Efficiency
of World Cropping Systems: The Relationship between Yield and Nitrogen
Input to Cropland. Environmental Research Letters. into a dataset
including all countries. Replacing Soviet Union by Russia and Yugoslavia
by Serbia without detailed disaggregation.

## Usage

``` r
convertLassaletta2014(x, subtype)
```

## Arguments

- x:

  data object that gets provided by wrapper function readSource

- subtype:

  budget provides the nr cropland budgets, fert_to_cropland the share of
  inorganic fertilizers being applied to croplands

## Value

Magpie object with results on country level.

## See also

[`readLassaletta2014()`](readLassaletta2014.md),
[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("Lassaletta2014", convert = TRUE)
} # }
```
