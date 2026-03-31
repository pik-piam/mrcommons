# calcEDGAR6

Creates an emission inventory based on the EDGAR5 and EDGAR6 database.

## Usage

``` r
calcEDGAR6(non_country = FALSE, nutrient = TRUE)
```

## Arguments

- non_country:

  can alternatively provide SEA and AIR emissions, which are global
  emissions that cannot be attributed to a country

- nutrient:

  if TRUE, nitrogen is reported as N and CO2 as C

## Value

List of magpie object with results on country level, weight on country
level, unit and description.

## Author

Benjamin Leon Bodirsky
