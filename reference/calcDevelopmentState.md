# calc

Provides development state of a country or region. We use worldbank
definitions by default: above 12746 USD per capita, its a high-income
country, below 1045 its a low-income country, in between its a
medium-income country.

## Usage

``` r
calcDevelopmentState(upper = 12746, lower = 1045)
```

## Arguments

- upper:

  Change upper limit (default: 12746, i.e. the upper threshold for 2013
  in current US\$)

- lower:

  Change lower limit (default: 1045, i.e. the lower threshold for 2013
  in current US\$)

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`mrdrivers::calcGDPpc()`](https://pik-piam.github.io/mrdrivers/reference/calcGDP.html)

## Author

Benjamin Leon Bodirsky, Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("DevelopmentState")
} # }
```
