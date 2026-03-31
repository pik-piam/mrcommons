# calcBiocharAttributes

Provides biochar attributes for different pyrolysis process conditions.
Currently covered are Dry Matter (DM), Carbon (C), and Generalizable
Energy (GE). Values are assembled from various literature sources.

## Usage

``` r
calcBiocharAttributes(processCond = "default")
```

## Arguments

- processCond:

  Selects the pyrolysis process conditions for which data is returned.
  Options: "all", "default", or subset of: "SP-400", "SP-500", "SP-650",
  "SP-800", "FP-500", "P-Woolf", "P-Buffi", "P-KonTiki".

## Value

List of magpie objects with results on global level, empty weight, unit
and description.

## See also

[`readPyrolysisConditions()`](readPyrolysisConditions.md)

## Author

Isabelle Weindl

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("BiocharAttributes", processCond = "all")
} # }
```
