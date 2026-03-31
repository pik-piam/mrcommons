# calcBiocharEfficiency

Calculates biochar conversion efficiencies for different pyrolysis
process conditions, using data from multiple literature sources. Yield
metrics are expressed as output biochar per biomass input on dry matter,
carbon, and energy basis. Output can also cover H/C ratio when
available.

## Usage

``` r
calcBiocharEfficiency(processCond = "default", outputType = "default")
```

## Arguments

- processCond:

  Selects the pyrolysis process conditions for which data is returned.
  Options: "all", "default", or subset of: "SP-400", "SP-500", "SP-650",
  "SP-800", "FP-500", "P-Woolf", "P-Buffi", "P-KonTiki".

- outputType:

  Selects the output metric for which data is returned. Options: "all",
  "default", or subset of: "dm_yield", "en_yield", "c_yield",
  "HC_ratio".

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
calcOutput("BiocharEfficiency", processCond = "all", outputType = "all")
} # }
```
