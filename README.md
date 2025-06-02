# MadRat commons Input Data Library

R package **mrcommons**, version **1.56.2**

[![CRAN status](https://www.r-pkg.org/badges/version/mrcommons)](https://cran.r-project.org/package=mrcommons) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3822009.svg)](https://doi.org/10.5281/zenodo.3822009) [![R build status](https://github.com/pik-piam/mrcommons/workflows/check/badge.svg)](https://github.com/pik-piam/mrcommons/actions) [![codecov](https://codecov.io/gh/pik-piam/mrcommons/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrcommons) [![r-universe](https://pik-piam.r-universe.dev/badges/mrcommons)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Provides useful functions and a common structure to all the
    input data required to run models like MAgPIE and REMIND of model
    input data.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrcommons")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Jan Philipp Dietrich <dietrich@pik-potsdam.de>.

## Citation

To cite package **mrcommons** in publications use:

Bodirsky B, Karstens K, Baumstark L, Weindl I, Wang X, Mishra A, Wirth S, Stevanovic M, Steinmetz N, Kreidenweis U, Rodrigues R, Popov R, Humpenoeder F, Giannousakis A, Levesque A, Klein D, Araujo E, Bleidorn E, Beier F, Oeser J, Pehl M, Leip D, Crawford M, Molina Bacca E, von Jeetze P, Martinelli E, Schreyer F, Soergel B, Sauer P, Hötten D, Hasse R, Abrahão G, Weigmann P, Dietrich J (2025). "mrcommons: MadRat commons Input Data Library." doi:10.5281/zenodo.3822009 <https://doi.org/10.5281/zenodo.3822009>, Version: 1.56.2, <https://github.com/pik-piam/mrcommons>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {mrcommons: MadRat commons Input Data Library},
  author = {Benjamin Leon Bodirsky and Kristine Karstens and Lavinia Baumstark and Isabelle Weindl and Xiaoxi Wang and Abhijeet Mishra and Stephen Wirth and Mishko Stevanovic and Nele Steinmetz and Ulrich Kreidenweis and Renato Rodrigues and Roman Popov and Florian Humpenoeder and Anastasis Giannousakis and Antoine Levesque and David Klein and Ewerton Araujo and Eva Bleidorn and Felicitas Beier and Julian Oeser and Michaja Pehl and Debbora Leip and Michael Crawford and Edna {Molina Bacca} and Patrick {von Jeetze} and Eleonora Martinelli and Felix Schreyer and Bjoern Soergel and Pascal Sauer and David Hötten and Robin Hasse and Gabriel Abrahão and Pascal Weigmann and Jan Philipp Dietrich},
  doi = {10.5281/zenodo.3822009},
  date = {2025-06-02},
  year = {2025},
  url = {https://github.com/pik-piam/mrcommons},
  note = {Version: 1.56.2},
}
```
