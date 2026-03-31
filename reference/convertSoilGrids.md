# convertSoilGrids

Convert SoilGrids content

## Usage

``` r
convertSoilGrids(x)
```

## Arguments

- x:

  magpie object provided by the read function

## Value

List of magpie objects with results on cellular level, weight, unit and
description.

## See also

[`readSoilGrids()`](readSoilGrids.md)

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("SoilGrids", subtype = "cstock_0_30", convert = TRUE)
} # }
```
