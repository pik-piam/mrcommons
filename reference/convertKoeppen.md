# convertKoeppen

Convert Koeppen climate zones on iso-country level

## Usage

``` r
convertKoeppen(x, subtype = "iso")
```

## Arguments

- x:

  magpie object provided by the read function

- subtype:

  Switch between different levels

## Value

List of magpie objects with results on country level

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("Koeppen", subtype = "iso", convert = TRUE)
} # }
```
