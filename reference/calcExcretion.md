# calcExcretion

calculates excretion during grazing, cropland-grazing, confinement and
collected for fuel. Based on MAgPIE Feed baskets, slaughter biomass and
simple allocation rules.

## Usage

``` r
calcExcretion(cellular = FALSE, cells = "lpjcell", attributes = "npk")
```

## Arguments

- cellular:

  if TRUE value is calculate and returned (set aggregate to FALSE!) on
  cellular level

- cells:

  Switch between "magpiecell" (59199) and "lpjcell" (67420)

- attributes:

  npk (default) or npkc (inclusing carbon) can be selected

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## See also

[`calcExcretionIPCC()`](calcExcretionIPCC.md)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("Excretion")
} # }
```
