# readLassaletta2014

reads nitrogen budgets for a country dataset from Lassaletta, L., G.
Billen, B. Grizzetti, J. Angalde, and J. Garnier. 2014. 50 Year Trends
in Nitrogen Use Efficiency of World Cropping Systems: The Relationship
between Yield and Nitrogen Input to Cropland. Environmental Research
Letters.

## Usage

``` r
readLassaletta2014(subtype = "budget")
```

## Arguments

- subtype:

  budget provides the nr cropland budgets, fert_to_cropland the sahre of
  inorganic fertilizers being applied to croplands

## Value

Magpie object with results on country level.

## See also

[`convertLassaletta2014()`](convertLassaletta2014.md),
[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Benjamin Leon Bodirsky, Felicitas Beier

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("Lassaletta2014", subtype = "budget", convert = FALSE)
} # }
```
