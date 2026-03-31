# calcAWMSconfShr

calculates the share of manure managed in different animal waste
management systems in confinements. Starting with IPCC 2005 values,
turning into scenarios for the future.

## Usage

``` r
calcAWMSconfShr(rev = 0.1)
```

## Arguments

- rev:

  revision number of madrat run

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`calcAWMSconfShrPast()`](calcAWMSconfShrPast.md)

## Author

Benjamin Leon Bodirsky. Modifications by Edna J. Molina Bacca

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("AWMSconfShr")
} # }
```
