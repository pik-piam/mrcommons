# readPRIMAPhist

Read in an PRIMAP-hist data csv file as magclass object.

## Usage

``` r
readPRIMAPhist(subtype)
```

## Arguments

- subtype:

  data subtype. available subtypes are:

  - hist

  - hist_no_ex

## Value

magpie object of the PRIMAP-hist data.

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Roman Popov

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readPRIMAPhist("PRIMAPhist", "hist")
} # }
```
