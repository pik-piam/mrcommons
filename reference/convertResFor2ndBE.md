# convertResFor2ndBE

Convert old ReMIND use of residues for 2nd generation bioenergy to
country level data

## Usage

``` r
convertResFor2ndBE(x, subtype = subtype)
```

## Arguments

- x:

  MAgPIE object containing original values

- subtype:

  oldReMIND, newAgriSupply

## Value

List of magpie objects with results on country level

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("ResFor2ndBE", subtype = "oldReMIND", convert = TRUE)
} # }
```
