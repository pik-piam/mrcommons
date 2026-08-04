# readWCDE

It reads the population projections by age, sex and education of the
Wittgenstein Centre Human Capital Data Explorer as downloaded by
[`downloadWCDE()`](downloadWCDE.md). The output format follows
[`readLutz2014()`](readLutz2014.md): a magpie object with the dimensions
country, year, scenario, sex, age and education in million people. In
addition to the education categories of Lutz2014, the datasets contain a
finer split of the highest levels ("Short Post Secondary", "Bachelor"
and "Master and higher"). These belong to a separate classification
level and do not sum to "Post Secondary". Age-education combinations
that do not occur (e.g. a young child with a university degree) are set
to 0, as in Lutz2014. Country-year slices that are entirely absent from
the source (the pre-2015 reconstruction is missing for some countries in
"epop_v2") are kept as NA. World and continental aggregates (UN M49
codes \>= 900) are removed.

## Usage

``` r
readWCDE(subtype = "epop_v3")
```

## Arguments

- subtype:

  "epop_v3" (projections 2020-2100, K.C. et al. 2024) or "epop_v2" (SSP2
  only, including the historical reconstruction 1950-2015, Lutz et al.
  2018)

## Value

magpie object with missing countries, which can be filled with the
function convertWCDE.

## See also

[`downloadWCDE()`](downloadWCDE.md), [`convertWCDE()`](convertWCDE.md),
[`readLutz2014()`](readLutz2014.md)
