# Read JRC IDEES

Read the [IDEES data base from
JRC](https://data.jrc.ec.europa.eu/dataset/jrc-10110-10001/resource/f590b6f1-60e5-49a6-a972-60bc2b2e34b3)
\#nolint

## Usage

``` r
readJRC_IDEES(subtype)
```

## Arguments

- subtype:

  one of

  - `'Emission'`: read worksheets from the Emission Balance files

  - `'Energy'`: read worksheets from the Energy Balance files

  - `'Industry'`: read worksheets from the Industry files

  - `'Transport'`: read worksheets from the Transport files

  - `'MBunkers'`: read worksheets from the Bunkers files

  - `'Residential'`: read worksheets from the Residential files

  - `'Tertiary'`: read worksheets from the Tertiary (Services and
    Agriculture) files append `'_2021'` to get the updated data from
    2021 (only available for some sectors)

## Value

A [`magpie`](https://rdrr.io/pkg/magclass/man/magclass-package.html)
object.

## Author

Michaja Pehl, Falk Benke, Robin Hasse
