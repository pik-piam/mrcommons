# calcResFor2ndBioengery

Calculates the supply potential of Crop Residues for 2nd generation
bioenergy for future and different ssp scenarios

## Usage

``` r
calcResFor2ndBioengery(products = "all", product_aggr = TRUE, add_off = FALSE)
```

## Arguments

- products:

  categorie (set) that should be reported, switch between "kres",
  "res_crop" (sum over all "kres"), "res_wood" and "all"

- product_aggr:

  boolean, if product set should be summed up

- add_off:

  add a column with empty supply for no residues available for 2nd gen
  BE

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

`calcResFor2ndBioengery()`

## Author

Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("ResFor2ndBioengery")
} # }
```
