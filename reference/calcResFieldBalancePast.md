# calcResFieldBalancePast

Calculates data for aboveground and belowground residues production with
other usage

## Usage

``` r
calcResFieldBalancePast(cellular = FALSE, products = "sum")
```

## Arguments

- cellular:

  If TRUE calculation and output on cellular level

- products:

  "sum" (default) or "kres"

## Value

data

## See also

[`madrat::calcOutput()`](https://rdrr.io/pkg/madrat/man/calcOutput.html),
[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Benjamin Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("ResFieldBalancePast")
} # }
```
