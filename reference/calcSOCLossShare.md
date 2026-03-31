# calcSOCLossShare

Calculates soil organic carbon loss share on cellular level

## Usage

``` r
calcSOCLossShare(
  subsystems = FALSE,
  rate = "change",
  factor = "ipccReduced",
  cells = "lpjcell"
)
```

## Arguments

- subsystems:

  if FALSE just generic values will be used per climate zone , if TRUE
  crop specific values will be reported, if aggregated crop specific
  factors will be aggregated using crop area

- rate:

  if change, change rates will be reported; if loss, loss rates will be
  reported

- factor:

  switch for different ipcc versions (ipccReduced, ipccReduced2019)

- cells:

  "magpiecell" for 59199 cells or "lpjcell" for 67420 cells

## Value

List of magpie objects with results on cellular level, weight, unit and
description.

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("SOCLossShare", aggregate = FALSE)
} # }
```
