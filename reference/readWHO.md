# Read WHO

Read-in WHO (World health organization) data files as magpie object. The
files contain information on physical inactivity

## Usage

``` r
readWHO(subtype)
```

## Arguments

- subtype:

  Type of WHO data that should be read. Includes
  physical_inactivity_adults and physical_inactivity_underaged

## Value

magpie object of the WHO data

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Benjamin Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource(type = "WHO", subtype = "physical_activity_adults")
} # }
```
