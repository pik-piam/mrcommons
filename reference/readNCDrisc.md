# readNCDrisc

Reads in data from the NCD risc consortium body height: Collaboration
(NCD-RisC), NCD Risk Factor. 2016. "A Century of Trends in Adult Human
Height." ELife 5 (July):e13410. https://doi.org/10.7554/eLife.13410.

## Usage

``` r
readNCDrisc(subtype)
```

## Arguments

- subtype:

  "height" for body height data

## Value

magpie object with the dataset downloaded. It contains missing values
and it is possible to replace them with the function convertNCDrisc

## See also

[`convertNCDrisc()`](convertNCDrisc.md)

## Author

Benjamin Leon Bodirsky
