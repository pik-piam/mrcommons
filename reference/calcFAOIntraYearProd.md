# calcFAOIntraYearProd

Distribute massbalanced or FAOSTAT staple production to monthly or
quarterly interval based on GGCMI crop calendar. Only national level
implemented for now as cellular production only available on 5 year time
steps due to memory. Assume rainfed crop calendar date for now.

## Usage

``` r
calcFAOIntraYearProd(
  day = "harvest_day",
  products = "kcr",
  frequency = "monthly",
  attribute = "dm"
)
```

## Arguments

- day:

  harvest_day (to market) or maturity_day (first mature)

- products:

  "kcr" or "staples" staples uses FAO production dataset instead of
  calcProduction to only give maize wheat soy and rice. Allows for more
  years. A bit of a David-specific subtype

- frequency:

  monthly or quarterly. Daily leads to memory limits.

- attribute:

  dm default. can only select one at a time due to memory

  \#' @seealso [`readGGCMICropCalendar`](readGGCMICropCalendar.md)

## Author

David Chen
