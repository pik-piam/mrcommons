% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcResDemand.R
\name{calcResDemand}
\alias{calcResDemand}
\title{calcResDemand}
\usage{
calcResDemand(cellular = FALSE, scenario = "dafault")
}
\arguments{
\item{cellular}{If TRUE calculation and output on cellular level}

\item{scenario}{define scenario switch for sensititvy analysis
for historical SOC budget}
}
\value{
List of magpie objects with results on country level, weight on country level, unit and description.
}
\description{
Calculates the demand for Crop Residues
}
\examples{
\dontrun{
calcOutput("ResDemand")
}
}
\seealso{
\code{\link[=calcResBiomass]{calcResBiomass()}}
}
\author{
Benjamin Leon Bodirsky
}
