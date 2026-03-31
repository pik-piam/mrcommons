# Read EUROSTAT_livestock

Read in EUROSTAT livestock data

## Usage

``` r
readEurostatLivestock(subtype)
```

## Arguments

- subtype:

  Type of FAO data that should be read. Available types are:

  - `EggPopProd` : Production of eggs for consumption and number of
    laying hens

  - `LayingHensPop` : Laying hens population - annual data

  - `PoultryPop` : Poultry - annual data

  - `PigPop` : Pig population - annual data

  - `BovinePop` : Bovine population - annual data

  - `Nuts2Pop` : Animal populations by NUTS 2 regions

  - `MeatProd` : Meat production and foreign trade - head - monthly data

  - `MilkNuts2Prod` : Production of cow's milk on farms by NUTS 2
    regions

  - `MilkProd` : Production and utilization of milk on the farm - annual
    data

## Value

EUROSTAT livestock data as MAgPIE object

## See also

[`madrat::readSource()`](https://rdrr.io/pkg/madrat/man/readSource.html)

## Author

David Hötten

## Examples

``` r
if (FALSE) { # \dontrun{
a <- readSource("EurostatLivestock", "MeatProd")
} # }
```
