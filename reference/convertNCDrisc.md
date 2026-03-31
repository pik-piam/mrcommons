# convertNCDrisc

Converts data from the NCD risc consortium body height: Collaboration
(NCD-RisC), NCD Risk Factor. 2016. "A Century of Trends in Adult Human
Height." ELife 5 (July):e13410. https://doi.org/10.7554/eLife.13410.

## Usage

``` r
convertNCDrisc(x, subtype)
```

## Arguments

- x:

  unconverted magpie object from read-script

- subtype:

  "height" for body height data. Missing data is replaced by
  non-population weighted global average

## Value

magpie object with a completed dataset.

## See also

`convertNCDrisc()`
