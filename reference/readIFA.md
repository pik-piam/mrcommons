# Read IFA

Read-in IFA (International Fertilizer Association) data .xlsx file as
magclass object

## Usage

``` r
readIFA(subtype)
```

## Arguments

- subtype:

  Type of IFA data that should be read. Available types are:

  - `consumption`: read in fertilizer_consumption.xlsx data

  - `production`: read in fertilizer_production.xlsx data

## Value

magpie object of the IFA data

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Lavinia Baumstark

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource(type = "IFA", subtype = "consumption")
} # }
```
