# calcEmisNitrogenPreagriculture

Calculates nitrogenous emissions Nitrogen emissions from soils under
100% natural cover (even for crop and urban) assuming a pre-agricultural
time.

## Usage

``` r
calcEmisNitrogenPreagriculture(cellular = FALSE, deposition = TRUE)
```

## Arguments

- cellular:

  cellular or country outputs

- deposition:

  if TRUE, losses include atmospheric deposition inputs that are lost
  afterwards. If false, only biological fixation is considered.

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
calcOutput("EmisNitrogenPreagriculture")
} # }
```
