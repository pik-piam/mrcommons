# MadRat commons Input Data Library

R package **mrcommons**, version **0.11.10**

[![Travis build status](https://travis-ci.com/pik-piam/mrcommons.svg?branch=master)](https://travis-ci.com/pik-piam/mrcommons) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3822009.svg)](https://doi.org/10.5281/zenodo.3822009) 

## Purpose and Functionality

Provides useful functions and a common structure to all the input data required to run models like MAgPIE and REMIND
    of model input data.


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

Bodirsky B, Karstens K, Baumstark L, Weindl I, Wang X, Mishra A, Wirth S, Stevanovic M, Steinmetz N, Kreidenweis U, Rodrigues R, Popov
R, Humpenoeder F, Giannousakis A, Levesque A, Klein D, Araujo E, Beier F, Oeser J, Pehl M, Leip D, Molina Bacca E, Martinelli E,
Schreyer F, Dietrich J (2020). _mrcommons: MadRat commons Input Data Library_. doi: 10.5281/zenodo.3822009 (URL:
https://doi.org/10.5281/zenodo.3822009), R package version 0.11.10, <URL: https://github.com/pik-piam/mrcommons>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrcommons: MadRat commons Input Data Library},
  author = {Benjamin Leon Bodirsky and Kristine Karstens and Lavinia Baumstark and Isabelle Weindl and Xiaoxi Wang and Abhijeet Mishra and Stephen Wirth and Mishko Stevanovic and Nele Steinmetz and Ulrich Kreidenweis and Renato Rodrigues and Roman Popov and Florian Humpenoeder and Anastasis Giannousakis and Antoine Levesque and David Klein and Ewerton Araujo and Felicitas Beier and Julian Oeser and Michaja Pehl and Debbora Leip and Edna {Molina Bacca} and Eleonora Martinelli and Felix Schreyer and Jan Philipp Dietrich},
  year = {2020},
  note = {R package version 0.11.10},
  doi = {10.5281/zenodo.3822009},
  url = {https://github.com/pik-piam/mrcommons},
}
```

