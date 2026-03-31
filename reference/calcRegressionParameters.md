# calcRegressionParameters

Writes down the equation parameters from various data sources

## Usage

``` r
calcRegressionParameters(regression = "bmi_shr")
```

## Arguments

- regression:

  bmi_shr: Shares of BMI withina population. schofield or
  FAO_WHO_UNU1985: calculates intake based on anthropometrics

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("RegressionParameters")
} # }
```
