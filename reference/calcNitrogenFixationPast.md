# calcNitrogenFixationPast

calculates fixation from freeliving bacteria and from nitrogen-fixing
crops

## Usage

``` r
calcNitrogenFixationPast(
  fixation_types = "both",
  sum_plantparts = TRUE,
  cellular = FALSE,
  irrigation = FALSE
)
```

## Arguments

- fixation_types:

  either "fixation_crops", "fixation_freeliving", or "both"

- sum_plantparts:

  if false, crop residues, belowground residues and harvested organ are
  reported separately

- cellular:

  cellular estimates optional

- irrigation:

  if TRUE, distinguishes irrigated and non-irrigated crops

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

`calcNitrogenFixationPast()`

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("calcNitrogenFixationPast")
} # }
```
