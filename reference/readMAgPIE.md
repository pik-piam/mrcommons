# Read MAgPIE data

Read-in MAgPIE data

## Usage

``` r
readMAgPIE(subtype)
```

## Arguments

- subtype:

  Either "EmiAirPoll", "macBase" or "co2tax"

## Value

magpie object

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Julian Oeser

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource(type = "MAgPIE", subtype = "EmiAPExo")
} # }
```
