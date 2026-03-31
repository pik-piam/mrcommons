# Read allometric coeffizients for residue to harvest translation

Read-in a file containing the allometric coeffizients of MAgPIE crop
types. Values are assembled from various literature sources, and the
weighting and allocation is done in the spreadsheet
crop_specifications\_\*.ods for different versions of the file

## Usage

``` r
readHI()
```

## Value

magpie object with the dimension crops and coeffizients

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

Benjamin Leon Bodrisky, Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("HI")
} # }
```
