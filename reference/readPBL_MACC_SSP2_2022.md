# Read in PBL MAC curves from Harmsen_et_al_2022 for different subtypes and subsets, using the baseline-dependent IMAGE SSP2 version

Read in PBL MAC curves from Harmsen_et_al_2022 for different subtypes
and subsets, using the baseline-dependent IMAGE SSP2 version

## Usage

``` r
readPBL_MACC_SSP2_2022(subtype, subset)
```

## Arguments

- subtype:

  data subtype.
  "ch4coal","ch4oil","ch4gas","ch4wstl","ch4wsts","ch4rice","ch4animals","ch4anmlwst",
  "n2otrans","n2oadac","n2onitac","n2ofert","n2oanwst","n2owaste"

- subset:

  data subset. "Default", "Optimistic", "Pessimistic"

## Value

magpie object of the PBL_MACC_SSP2_2022 data

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Michael Windisch, Florian Humpenoeder, Gabriel Abrahao
