# downloadWCDE

Downloads population data by age, sex and education from the
Wittgenstein Centre Human Capital Data Explorer (WCDE,
https://dataexplorer.wittgensteincentre.org). The batch files provided
for the wcde R package are used (one file per scenario).

Subtype "epop_v3" contains the updated SSP projections (2020-2100) of
the Wittgenstein Centre Data Explorer Version 3 (K.C. et al. 2024). The
download is pinned to version 3.3 to keep results reproducible when
newer minor versions are released. Subtype "epop_v2" contains the
previous release (Version 2, Lutz et al. 2018), which includes the
historical reconstruction going back to 1950. As historical estimates do
not differ between scenarios, only SSP2 is downloaded for "epop_v2".

Additional scenarios (e.g. the SSP2 zero migration variant, scenario
code 22) can be added to the scenario vectors below.

## Usage

``` r
downloadWCDE(subtype = "epop_v3")
```

## Arguments

- subtype:

  "epop_v3" or "epop_v2"

## See also

[`madrat::downloadSource()`](https://rdrr.io/pkg/madrat/man/downloadSource.html)
[`readWCDE()`](readWCDE.md)

## Author

Benjamin Leon Bodirsky
