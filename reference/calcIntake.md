# calcIntake

it computes the total intake kcal/day procapita through the population
dataset by Lutz 2014, height and BMI data of NCDrisc, and phyiscal
inactivity levels based on Halal et al.

## Usage

``` r
calcIntake(
  convert = TRUE,
  modelinput = FALSE,
  standardize = FALSE,
  method = "FAO_WHO_UNU1985"
)
```

## Arguments

- convert:

  if TRUE, Lutz data is converted (interpolated completed etc)

- modelinput:

  if TRUE, data is aggregated to country totals for model input

- standardize:

  if FALSE, no standardization. if "recommendations", the US
  recommendations are used. if BMI, a normal BMI is used.

- method:

  method for calculating intake: either FAO_WHO_UNU1985 for estimates
  based on height and bodyweight, schofield for just bodyweight, or
  HHS_USDA for recommended values for US-americans

## Value

total "healthy" intake kcal/day procap for each countries divided by sex
and 8 age groups.

## Author

Benjamin Leon Bodirsky
