# readNitrogenBoundariesGridded

Read the grid-level regional nitrogen boundary datasets from
Schulte-Uebbing et al. (2022). For the moment, this only uses the
critical N surplus in all agricultural land (arable + intensively
managed grassland) in view of all thresholds simultaneously, measured in
kg N per ha per yr.

## Usage

``` r
readNitrogenBoundariesGridded()
```

## Value

A MAgPIE object with the critical nitrogen surplus in kg N per ha per yr
in 2010

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Michael S. Crawford

## Examples

``` r
if (FALSE) { # \dontrun{
 a <- readSource("NitrogenBoundariesGridded")
} # }
```
