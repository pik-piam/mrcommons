# calcTemperature

calculates average monthly temperature on different landuse types

## Usage

``` r
calcTemperature(landusetypes = "all", months = FALSE, convert = TRUE)
```

## Arguments

- landusetypes:

  all or only one (to save computation memory)

- months:

  FALSE for yearly average, TRUE for monthly values

- convert:

  FALSE for raw values of temperature, TRUE add temperature of 15
  degrees for countries without observations or land mass.

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("Temperature")
} # }
```
