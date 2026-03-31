# calcIPCCfracLeach

calculates the leaching rate FRAC_LEACH as defined by the IPCC
Guidelines for National Greenhouse Gas Inventories 2006. We use the
approach used by Canada, see Velthof, Gerardus Lambertus, and J.
Mosquera Losada. 2011. Calculation of Nitrous Oxide Emission from
Agriculture in the Netherlands: Update of Emission Factors and Leaching
Fraction. Alterra. http://library.wur.nl/WebQuery/wurpubs/406284.

## Usage

``` r
calcIPCCfracLeach(cellular = TRUE)
```

## Arguments

- cellular:

  if true, returned on cell level

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`madrat::calcOutput()`](https://rdrr.io/pkg/madrat/man/calcOutput.html)

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
a <- calcOutput("IPCCfracLeach", cellular = FALSE)
} # }
```
