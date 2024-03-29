% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcClimateClass.R
\name{calcClimateClass}
\alias{calcClimateClass}
\title{calcClimateClass}
\usage{
calcClimateClass(datasource = "koeppen", cells = "magpiecell")
}
\arguments{
\item{datasource}{select source from:
- koeppen for Koeppen Geiger Classification
http://koeppen-geiger.vu-wien.ac.at/
- ipcc, ipccReduced, ipccReduced2019 for IPCC Guideline climate classification}

\item{cells}{"magpiecell" for 59199 cells or "lpjcell" for 67420 cells}
}
\value{
Clustered MAgPIE object on requested resolution
}
\description{
fraction of a cell belonging to a given climate classification based on
different climate cliassification schemes
}
\examples{
\dontrun{
calcOutput("ClimateClass", aggregate = FALSE)
}

}
\author{
Abhijeet Mishra, Kristine Karstens
}
