# Read IEA

Read-in an IEA csv file as magpie object

## Usage

``` r
readIEA(subtype)
```

## Arguments

- subtype:

  data subtype. Either "EnergyBalances", "EnergyBalances-latest", or
  "Emissions".

  - "EnergyBalances": IEA energy balances until 2022 (2023 incomplete),
    data updated in Sep 2024, the current default for REMIND input data

  - "EnergyBalances-latest": IEA energy balances until 2022 (2023
    incomplete), data updated in Sep 2024, currently same as default

## Value

magpie object of the IEA

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Anastasis Giannousakis, Lavinia Baumstark, Renato Rodrigues, Falk Benke

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource(type = "IEA", subtype = "EnergyBalances")
} # }
```
