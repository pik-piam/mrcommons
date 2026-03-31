# calcIntakeBodyweight

it computes the food intake pro capita through the bodyweight and the
activity level. First it computes the basal metabolic rate (bmr) through
the Schofield equation and then the estimated energy required (eer)
depending on the activitiy level by FAO/WHO/UNU tables (Human Energy
Requirments, Rome 2004)

## Usage

``` r
calcIntakeBodyweight(
  bodyweight,
  bodyheight = NULL,
  inactivity,
  tmean = NULL,
  method = NULL
)
```

## Arguments

- bodyweight:

  bodyweight in kg per capita or "standardized" for assuming standard
  values

- bodyheight:

  for mehthod FAO_WHO_UNU1985

- inactivity:

  Share of population inactive, provided as magpie object with different
  age groups

- tmean:

  mean annual temperature

- method:

  method for calculating intake: either FAO_WHO_UNU1985 for estimates
  based on height and bodyweight, schofield for just bodyweight, or
  HHS_USDA for recommended values for US-americans

## Author

Eleonora Martinelli
