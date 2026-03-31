# calcFoodSupplyPast

Calculates the food supply (as defined by FAO, including intake and
household waste) for the past.

## Usage

``` r
calcFoodSupplyPast(
  per_capita = TRUE,
  products = NULL,
  product_aggr = FALSE,
  populationweight = "PopulationPast",
  faoVersion = "join2010",
  attributes = c("kcal", "protein", "wm")
)
```

## Arguments

- per_capita:

  if true, calculates per capita demand per day, otherwhise total demand
  per year

- products:

  a set with the products that shall be provided, e.g. kall. If NULL,
  the products are provided that are in the primary data

- product_aggr:

  if TRUE, all products are summed up, if "maingroups" products are
  summed over livestock products, staples and vegfruits.

- populationweight:

  datasource of populationweight: FAO can be selected in order to better
  meet exact values. Normal datasource is PopulationPast

- faoVersion:

  which version of FAO food balances to use in mass balance

- attributes:

  attributes of different products,i.e., kcal,protein,wm

## Value

List of magpie objects with results on country level, weight on country
level, unit and description.

## See also

[`mrfaocore::calcFAOharmonized()`](https://rdrr.io/pkg/mrfaocore/man/calcFAOharmonized.html)

## Author

Benjamin Leon Bodirsky, Xiaoxi Wang

## Examples

``` r
if (FALSE) { # \dontrun{
calcOutput("FoodSupplyPast")
} # }
```
