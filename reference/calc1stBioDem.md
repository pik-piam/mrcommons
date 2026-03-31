# calc1stBioDem

Calculates projections of first generation biofuels demand,including
biogas, bioethamol and biodiesel, from IEA database. The unit is
Petajoule.

## Usage

``` r
calc1stBioDem(subtype = "all")
```

## Arguments

- subtype:

  all per default. ethanol_oils for selecting 1st gen crop types
  relevant for REMIND input.

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`calc1stBioenergyPast()`](calc1stBioenergyPast.md)

## Author

Xiaoxi Wang, David Klein

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("1stBioDem")
} # }
```
