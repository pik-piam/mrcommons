# convertGTAPv8v9

disaggregates country groups from GTAP according to GDP share

## Usage

``` r
convertGTAPv8v9(x, subtype)
```

## Arguments

- x:

  unconverted magpie object from read-script

- subtype:

  GTAP header that should be read

## Value

Data as MAgPIE object with common country list

## Author

Debbora Leip, David M. Chen

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("convertGTAPv8v9", "81:SF01", convert = TRUE)
} # }
```
