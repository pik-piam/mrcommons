# calcIOEdgeBuildings

Calculates buildings-related energy flows from the IEA energy balances.
'output_EDGE_buildings' is a key input to EDGE-Buildings providing the
historic final energy demand from buildings. 'output_EDGE' does the same
for buildings and industry together.

## Usage

``` r
calcIOEdgeBuildings(
  subtype = c("output_EDGE", "output_EDGE_buildings"),
  ieaVersion = c("default", "latest")
)
```

## Arguments

- subtype:

  Data subtype. See default argument for possible values.

- ieaVersion:

  Release version of IEA data, either 'default' (vetted and used in
  REMIND) or 'latest'.

## Value

IEA data as MAgPIE object aggregated to country level

## Author

Pascal Sauer, Anastasis Giannousakis, Robin Hasse

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings")
} # }
```
