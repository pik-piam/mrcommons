# calcResBiomass

Provides MAgPIE-FEED data for aboveground and belowground residues
biomass

## Usage

``` r
calcResBiomass(
  cellular = FALSE,
  cells = "lpjcell",
  plantparts = "both",
  irrigation = FALSE,
  attributes = "all"
)
```

## Arguments

- cellular:

  If TRUE calculation and output on cellular level

- cells:

  Switch between "magpiecell" (59199) and "lpjcell" (67420)

- plantparts:

  both, ag (aboveground) or belowground (bg). Both can have memory
  problems for cellular outputs

- irrigation:

  if TRUE, distinguishes irrigated and non-irrigated crops

- attributes:

  in dm, wm, ge, nr, p, k

## Value

MAgPIE-FEED data for ProdResAg and corresonding weights as a list of two
MAgPIE objects

## See also

[`madrat::calcOutput()`](https://rdrr.io/pkg/madrat/man/calcOutput.html),
[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Lavinia Baumstark, Isabelle Weindl, Benjamin Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("ResBiomass")
} # }
```
