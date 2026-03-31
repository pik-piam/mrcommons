# Read EU Reference Scenario

Read EU Reference Scenario .xlsx file as magpie object

## Usage

``` r
readEU_ReferenceScenario(subtype)
```

## Arguments

- subtype:

  data subtype. Either "techAssump.\*", "2016" or "2020"

## Value

magpie object of EU reference scenario data by country. Units follow
REMIND report conventions and conversion factor is defined in
EU_ReferenceScenario2REMIND.xlsx file.

## Author

Renato Rodrigues, Falk Benke, Robin Hasse

## Examples

``` r
if (FALSE) { # \dontrun{
test <- readSource("EU_ReferenceScenario", subtype = "2020", convert = FALSE)
} # }
```
