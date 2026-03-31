# readGGCMICropCalendar

Reads in GGCMI fraction of Harvested Area masks for rice 1 and rice 2
(other crops available too, see path in download function), or other
variables available in the GGCMI crop calendar.

## Usage

``` r
readGGCMICropCalendar(subtype = "fraction_of_harvested_area")
```

## Arguments

- subtype:

  variable or vector of variables to read from the crop calendar set.
  Options:
  ("planting_day","maturity_day","fraction_of_harvested_area","cal"
  (which is a combination of planting_day, maturity_day and
  harvest_day)), wheat areas and rice_areas

## Value

MAgPIE object with the requested data

## Author

David M Chen, Edna Molina Bacca
