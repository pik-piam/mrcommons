% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcAtmosphericTransboundaryRedepositionShare.R
\name{calcAtmosphericTransboundaryRedepositionShare}
\alias{calcAtmosphericTransboundaryRedepositionShare}
\title{calcAtmosphericRedepositionShare}
\usage{
calcAtmosphericTransboundaryRedepositionShare(
  maxshare = 0.8,
  scenario = "rcp45"
)
}
\arguments{
\item{maxshare}{the maximum amount of emissions deposited within the same cell or country.
The remainder will be handled as global emission}

\item{scenario}{scenario}
}
\value{
List of magpie object with results on country level, weight on country level, unit and description.
}
\description{
Calculates share of volatilised nitrogen emissions that is redeposited on different land types.
}
\examples{
\dontrun{
calcOutput("AtmosphericRedepositionShare")
}

}
\author{
Benjamin Leon Bodirsky
}
