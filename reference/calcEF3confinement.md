# calcEF3confinement

Emission factors for nitrogenous emissions in livestock confinements

## Usage

``` r
calcEF3confinement(products = "magpie", selection = "n_pollutants_direct")
```

## Arguments

- products:

  Either livestock products in MAgPIE or IPCC products

- selection:

  defaults to n_pollutants_direct

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("EF3confinement")
} # }
```
