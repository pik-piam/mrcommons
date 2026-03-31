# readHerridge

Reads a dataset containing values for biological nitrogen fixation in
agricultural systems. Source: Herridge D. F., Peoples M. B., Boddey R.
M.: Global inputs of biological nitrogen fixation in agricultural
systems

## Usage

``` r
readHerridge(subtype = NULL)
```

## Arguments

- subtype:

  a subtype for the calculation

## Value

A MAgPIE object containting the share of Nr derived from

- ndfa: fixation for each country and each commodity.

- freeliving: fixation by free living agents

## Details

Availables Subtypes:

- ndfa: National values for Plant associated fixation

- freeliving: Global values for free living agents

## Author

Stephen Wirth, Jan Philipp Dietrich

## Examples

``` r
if (FALSE) { # \dontrun{
x <- readSource("Herridge", "ndfa")
x <- readSource("Herridge", "freeliving", convert = F)
} # }
```
