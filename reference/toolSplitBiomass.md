# Split Biomass into modern and traditional

We assume that below a given GDP/cap level, all biomass is traditional
and above a higher limit, all biomass is modern with a linear transition
between the limits.

## Usage

``` r
toolSplitBiomass(
  x,
  gdppop,
  split = "biomass",
  into = c("biotrad", "biomod"),
  dim = 3.1,
  limits = c(12300, 18500)
)
```

## Arguments

- x:

  MagPIE object including biomass data

- gdppop:

  MagPIE object with GDP/cap data

- split:

  character, name of item to split

- into:

  character vector of length two with the names of the split items

- dim:

  dimension of `x` with the item to split

- limits:

  numeric vector of length two with the corresponding GDP/cap limits.
  The default values used to be 10k and 15k USD/cap converted from 2005
  to 2017 dollars.

## Value

MagPIE object including split biomass data

## Author

Robin Hasse
