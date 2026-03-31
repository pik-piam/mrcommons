# readEDGARfood

read Edgar-food v6 food system emission data

## Usage

``` r
readEDGARfood(subtype)
```

## Arguments

- subtype:

  Type of data that should be read

  - `foodSystemEmi`: Total food system emissions of different countries

  - `foodSystemShare`: Share of food system emissions in total emissions

  - `foodSystemSector`: Food system emissions separated by country,
    sector and substance

## Value

A magpie object with foodsystem total emissions, emission shares or
sector and substance specific emission, depending on subtype

## Author

David Hötten
