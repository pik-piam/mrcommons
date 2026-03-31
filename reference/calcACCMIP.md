# calcACCMIP

reads in the ACCMIP atmospheric deposition database. Speeds up caching

## Usage

``` r
calcACCMIP(glo_incl_oceans = FALSE)
```

## Arguments

- glo_incl_oceans:

  if true, a global value will be returned that also includes deposition
  on oceans and should be equivalent to total emissions.

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`calcAtmosphericDeposition()`](calcAtmosphericDeposition.md)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("ACCMIP")
} # }
```
