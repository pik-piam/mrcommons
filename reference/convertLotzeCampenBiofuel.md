# convertLotzeCampenBiofuel

Converts the Data from Lotze Campen et al. 2014. to fit the common
country list. Source: Lotze Campen et al. 2014. "Impacts of increased
bioenergy demand on global food markets: an AgMIP economic model
intercomparison" Agricultural Economics 45 (103-116).
doi:10.1111/agec.12092.

## Usage

``` r
convertLotzeCampenBiofuel(x)
```

## Arguments

- x:

  MAgPIE object to be converted

## Value

A MAgPIE object containing future trends in first generation bioenergy
demand in Petajoules as magpie object for each country for biodiesel and
ethanol.

## Author

Ewerton Araujo

## Examples

``` r
if (FALSE) { # \dontrun{
x <- readSource("LotzeCampenBiofuel")
} # }
```
