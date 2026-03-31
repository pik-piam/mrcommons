# calcClimateClass

fraction of a cell belonging to a given climate classification based on
different climate cliassification schemes

## Usage

``` r
calcClimateClass(datasource = "koeppen", cells = "magpiecell")
```

## Arguments

- datasource:

  select source from: - koeppen for Koeppen Geiger Classification
  http://koeppen-geiger.vu-wien.ac.at/ - ipcc, ipccReduced,
  ipccReduced2019 for IPCC Guideline climate classification

- cells:

  "magpiecell" for 59199 cells or "lpjcell" for 67420 cells

## Value

Clustered MAgPIE object on requested resolution

## Author

Abhijeet Mishra, Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("ClimateClass", aggregate = FALSE)
} # }
```
