# Read in IPCC emissions

Read in IPCC data:

- Read in IPCC emissions from livestock and manure management. Source:
  IPCC Guidelines for National Greenhouse Gas Inventories (2006);
  Chapter 10: Emissions from Livestock and Manure Management.

- Read in IPCC emissions from Lime and urea application. Source: IPCC
  Guidelines for National Greenhouse Gas Inventories (2006); Chapter 11:
  N2O Emissions from managed Soils and Co2 Emissions from Lime and Urea
  Application.

- Read in IPCC efficiency factors for burning of residue. Source: IPCC
  Guidelines for Natinal Greenhouse Gas Inventories (2006); Chapter 02:
  Generic Methodologies applicable to multiple Land-use Categories.

- Read in soil related stock change factors for carbon and manure
  parameterization. Source: IPCC Guidelines for National Greenhouse Gas
  Inventories (2006); Chapter 5: Cropland.

## Usage

``` r
readIPCC(subtype)
```

## Arguments

- subtype:

  data subtype

## Value

magpie object of the IPCC data

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Nele Steinmetz, Stephen Wirth, Jan Philipp Dietrich, Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("IPCC", "awmsShr")
a <- readSource("IPCC", "awmsEfCh4")
a <- readSource("IPCC", "awmsParCh4")
a <- readSource("IPCC", "nExcrRate")
a <- readSource("IPCC", "awmsconfef3", convert = FALSE)
a <- readSource("IPCC", "fracgasms", convert = FALSE)
a <- readSource("IPCC", "fraclossms", convert = FALSE)
a <- readSource("IPCC", "emissionfactors", convert = FALSE)
a <- readSource("IPCC", "rescombusteff", convert = FALSE)
a <- readSource("IPCC", "efnsoil", convert = FALSE)
} # }
```
