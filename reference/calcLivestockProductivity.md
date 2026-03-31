# Calculate Yields for Livestock

Provides livestock yields for historical and future years.

## Usage

``` r
calcLivestockProductivity(future = TRUE)
```

## Arguments

- future:

  if TRUE calculates constant future and linear trends based on SSP
  Expert guesses

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`madrat::calcOutput()`](https://rdrr.io/pkg/madrat/man/calcOutput.html),
[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Isabelle Weindl, Lavinia Baumstark, Stephen Wirth

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("LivestockProductivity")
} # }
```
