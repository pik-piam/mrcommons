# readACCMIP

Read in data from the Atmospheric Chemistry and Climate Model
Intercomparison Project

## Usage

``` r
readACCMIP(subtype = NULL)
```

## Arguments

- subtype:

  data subtype. available subtypes are:

  - nhx_1850

  - noy_1850

  - sox_1850

  - nhx_1980

  - noy_1980

  - sox_1980

  - nhx_2000

  - noy_2000

  - sox_2000

  - nhx_26_2030

  - nhx_45_2030

  - nhx_85_2030

  - noy_26_2030

  - noy_45_2030

  - noy_85_2030

  - sox_26_2030

  - sox_45_2030

  - sox_85_2030

  - nhx_26_2100

  - nhx_45_2100

  - nhx_85_2100

  - noy_26_2100

  - noy_45_2100

  - noy_85_2100

  - sox_26_2100

  - sox_45_2100

  - sox_85_2100

## Value

magpie object of the ACCMIP data. Unit is t NO2-N per ha per year, or t
NH3-N per ha per year,...

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Roman Popov

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readACCMIP("ACCMIP", "nhx_2000")
a <- readACCMIP("ACCMIP", "sox_26_2030")
} # }
```
