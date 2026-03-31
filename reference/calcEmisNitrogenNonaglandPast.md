# calcEmisNitrogenNonaglandPast

Calculates nitrogenous emissions from non-agricultural land for the
historical period

## Usage

``` r
calcEmisNitrogenNonaglandPast(method = "Nsurplus")
```

## Arguments

- method:

  Method for calculating Atmospheric deposition: Nsurplus2 and Nsurplus
  are based on deposition rates based on own emission calculations after
  2 or after 1 iteration, respectively.

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## See also

[`calcEmisNitrogenPast()`](calcEmisNitrogenPast.md),
[`calcExcretion()`](calcExcretion.md)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("EmisNitrogenPasturePast")
} # }
```
