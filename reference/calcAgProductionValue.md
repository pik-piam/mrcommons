# calcAgProductionValue

Calculate FAO Value Of Production

## Usage

``` r
calcAgProductionValue(datasource = "FAO")
```

## Arguments

- datasource:

  Currently available: `"FAO"`

## Value

FAO Value Of Production as a list of MAgPIE objects

## See also

[`madrat::calcOutput()`](https://rdrr.io/pkg/madrat/man/calcOutput.html),
[`mrfaocore::readFAO()`](https://rdrr.io/pkg/mrfaocore/man/readFAO.html),
[`mrfaocore::convertFAO()`](https://rdrr.io/pkg/mrfaocore/man/convertFAO.html),
[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Roman Popov, Mishko Stevanovic, Patrick v. Jeetze

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput("AgProductionValue", datasource = "FAO")
} # }
```
