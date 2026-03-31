# calcSOMlossN

calculates effect on N from Soil Organic Matter loss

## Usage

``` r
calcSOMlossN(cellular = FALSE, cells = "lpjcell")
```

## Arguments

- cellular:

  if TRUE cellular level is returned

- cells:

  "magpiecell" for 59199 cells or "lpjcell" for 67420 cells

## Value

List of magpie object with results on country or cellular level, weight
on cellular level, unit and description.

## Author

Benjamin Leon Bodirsky,

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("SOMlossN")
} # }
```
