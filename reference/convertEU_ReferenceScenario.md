# Convert EU Reference Scenario

Converts EU Reference Scenario magpie object into appropriate form for
the REMIND model

## Usage

``` r
convertEU_ReferenceScenario(x, subtype)
```

## Arguments

- x:

  EU Reference Scenario magpie object derived from
  readEU_ReferenceScenario function

- subtype:

  data subtype. Either "techAssump.\*", "2016" or "2020"

## Value

converted EU Reference Scenario magpie object

## Author

Renato Rodrigues, Falk Benke, Robin Hasse

## Examples

``` r
if (FALSE) { # \dontrun{
test <- readSource("EU_ReferenceScenario", subtype = "2020", convert = TRUE)
} # }
```
