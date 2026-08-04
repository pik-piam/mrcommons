# calcDemography

Population by age, sex and education.

With version = "WCDE" the data is taken from the Wittgenstein Centre
Human Capital Data Explorer: the historical reconstruction (1965-2015)
from version 2 (Lutz et al. 2018) is combined with the updated SSP
projections (2020-2100) from version 3 (K.C. et al. 2024). Education is
aggregated to the six attainment categories plus "Under 15" that are
also used in the legacy Lutz2014 data (the finer post-secondary split of
the WCDE data is dropped). With version = "Lutz2014" the previous data
source is used instead.

Population is divided by sex male (M), female (F) and, before
recalibration, both (B), by five-year age groups and by education.

## Usage

``` r
calcDemography(convert = TRUE, education = TRUE, version = "Lutz2014")
```

## Arguments

- convert:

  if TRUE, country coverage is completed (convert functions) and the
  demography is recalibrated to the population scenarios of
  calcOutput("Population").

- education:

  if FALSE, no education dimension will be provided

- version:

  "WCDE" (default, Wittgenstein Centre v2 history + v3 projections) or
  "Lutz2014" (legacy data source)
