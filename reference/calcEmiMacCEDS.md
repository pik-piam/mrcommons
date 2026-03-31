# Calculate baseline emissions for maccs (mostly) from CEDS2024 data

Provides REMIND data for baseline emissions for maccs from CEDS2024 Note
that this can't fully replace the old EDGAR data, as it doesn't include
land use CO2 emissions. These are not currently actually used in REMIND
by default as the LUC CO2 MACC is off, but it's still not officially
deprecated.

## Usage

``` r
calcEmiMacCEDS(baseyear = 2020)
```

## Arguments

- baseyear:

  year to take as a reference from CEDS, ignored for the EDGAR2005 LUC
  CO2 emissions

## Value

REMIND data for baseline emissions for maccs and corresonding weights
(NULL) as a list of two MAgPIE objects

## Details

Therefore, the CO2 from LUC is read from EDGAR with a 2005, a
potentially different baseyear than the one chosen in the parameter

## See also

[`madrat::calcOutput()`](https://rdrr.io/pkg/madrat/man/calcOutput.html),
[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Gabriel Abrahao
