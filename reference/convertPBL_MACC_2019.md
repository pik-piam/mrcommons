# Convert subtypes of the PBL_MACC_2019 data

Convert subtypes from PBL_MACC_2019 to data on ISO country level.#'

## Usage

``` r
convertPBL_MACC_2019(x, subtype)
```

## Arguments

- x:

  MAgPIE object containing PBL_MACC_2019 data on region level

- subtype:

  data subtype.
  "ch4coal","ch4oil","ch4gas","ch4wstl","ch4wsts","ch4rice","ch4animals","ch4anmlwst",
  "n2otrans","n2oadac","n2onitac","n2ofert","n2oanwst","n2owaste",
  "HFC_tot", "SF6_tot", "PFC_tot" or "baseline_sources"

## Value

PBL_MACC_2019 data as MAgPIE object for all subtypes aggregated to
country level

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Florian Humpenoeder
