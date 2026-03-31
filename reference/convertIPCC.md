# Convert subtypes of the IPCC data

Convert subtypes on ISO country level.

## Usage

``` r
convertIPCC(x, subtype)
```

## Arguments

- x:

  MAgPIE object containing IPCC data on region level

- subtype:

  data subtype. Either "awmsShr", "awmsEfCh4", "awmsParCh4", "nExcrRate"

## Value

IPCC data as MAgPIE object for all subtypes on country level

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Nele Steinmetz

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("IPCC", "awmsShr")
a <- readSource("IPCC", "awmsEfCh4")
a <- readSource("IPCC", "awmsParCh4")
a <- readSource("IPCC", "nExcrRate")
} # }
```
