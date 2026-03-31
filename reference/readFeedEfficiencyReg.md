# Read in regression coefficients for feed efficiency

Read in csv file containing coefficients of non-linear regression
analysis for the calculation of future feed efficiencies of feed baskets
dependent on livestock productivity trends

## Usage

``` r
readFeedEfficiencyReg()
```

## Value

MAgPIE object containing regression coefficients

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Isabelle Weindl

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("FeedEfficiencyReg")
} # }
```
