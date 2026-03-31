# convertSSPResults

Disagregates from SSP regions to ISO countries

## Usage

``` r
convertSSPResults(x)
```

## Arguments

- x:

  object coming from read function

## Value

MAgPIE object with ISO countries with all indicators for which
disaggregation weight was found

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Abhijeet Mishra, Benjamin Leon Bodirsky, Florian Humpenoeder

## Examples

``` r
if (FALSE) { # \dontrun{
readSource("SSPResults", aggregate = TRUE)
} # }
```
