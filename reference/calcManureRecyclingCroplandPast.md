# calcManureRecyclingCroplandPast

calculates manure recycling to cropland based on excretions, animal
waste management types (and their shares per country) and emission
factors for nitrogenous emissions in livestock confinements

## Usage

``` r
calcManureRecyclingCroplandPast(
  products = "sum",
  cellular = FALSE,
  cells = "lpjcell"
)
```

## Arguments

- products:

  "sum" (default) or "kli"

- cellular:

  if TRUE value is calculate and returned (set aggregate to FALSE!) on
  cellular level

- cells:

  Switch between "magpiecell" (59199) and "lpjcell" (67420)

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## See also

[`calcExcretion()`](calcExcretion.md)

## Author

Benjamin Leon Bodirsky, Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("ManureRecyclingCroplandPast")
} # }
```
