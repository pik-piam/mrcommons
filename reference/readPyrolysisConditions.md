# readPyrolysisConditions

Reads in parameter specifications for different pyrolysis process
conditions from several literature sources, which are available as
separate source data files.

## Usage

``` r
readPyrolysisConditions(subtype = "Schmidt_2019")
```

## Arguments

- subtype:

  Available subtypes: "Schmidt_2019", "Woolf_2014", "Buffi_2024" and
  "Cornelissen_2016"

## Value

Magpie object with the dimensions "properties" and "process_cond"

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Isabelle Weindl

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("PyrolysisConditions")
} # }
```
