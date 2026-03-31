# Convert JRC IDEES data

Missing data for EU-28 countries is added, by distributing the
difference of `EU28` and the sum of country-values based on countries
share in EU-28 GDP.

## Usage

``` r
convertJRC_IDEES(x, subtype)
```

## Arguments

- x:

  A [`magpie`](https://rdrr.io/pkg/magclass/man/magclass-package.html)
  object returned from [`readJRC_IDEES()`](readJRC_IDEES.md).

- subtype:

  character, subtype of [`readJRC_IDEES()`](readJRC_IDEES.md)

## Value

A [`magpie`](https://rdrr.io/pkg/magclass/man/magclass-package.html)
object.

## See also

[`readJRC_IDEES()`](readJRC_IDEES.md)

## Author

Michaja Pehl, Robin Hasse
