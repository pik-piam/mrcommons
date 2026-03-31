# calcBiomeType

Returns fraction of spatial unit (cell) belonging to a biome type of
each biogeographic realm. The classification is based on data from 'the
nature conservancy'
(https://geospatial.tnc.org/datasets/b1636d640ede4d6ca8f5e369f2dc368b/about)
originally developed by Olson et al. (2001), BioScience.

## Usage

``` r
calcBiomeType(cells = "lpjcell")
```

## Arguments

- cells:

  magpiecell (59199 cells) or lpjcell (67420 cells)

## Value

List with a magpie object

## See also

[`readTNC2019`](readTNC2019.md)

## Author

Patrick v. Jeetze

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("BiomeType", aggregate = FALSE)
} # }
```
