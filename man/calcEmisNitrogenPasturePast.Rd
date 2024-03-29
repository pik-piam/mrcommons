% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcEmisNitrogenPasturePast.R
\name{calcEmisNitrogenPasturePast}
\alias{calcEmisNitrogenPasturePast}
\title{calcEmisNitrogenPasturePast}
\usage{
calcEmisNitrogenPasturePast(method = "IPCC")
}
\arguments{
\item{method}{IPCC: emissions are calculated according the the IPCC 2006 National Guidelines
for Greenhouse Gas Inventories. Nsurplus: Emissions in 2005 are calculated according to IPCC,
and the scaled with nitrogen losses from croplands.}
}
\value{
List of magpie object with results on country level, weight on country level, unit and description.
}
\description{
Calculates nitrogenous emissions from pastures for the historical period
}
\examples{
\dontrun{
calcOutput("EmisNitrogenPasturePast")
}

}
\seealso{
\code{\link[=calcEmisNitrogenPast]{calcEmisNitrogenPast()}},
\code{\link[=calcExcretion]{calcExcretion()}}
}
\author{
Benjamin Leon Bodirsky
}
