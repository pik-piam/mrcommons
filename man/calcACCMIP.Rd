% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcACCMIP.R
\name{calcACCMIP}
\alias{calcACCMIP}
\title{calcACCMIP}
\usage{
calcACCMIP(glo_incl_oceans = FALSE)
}
\arguments{
\item{glo_incl_oceans}{if true, a global value will be returned that also includes deposition on oceans
and should be equivalent to total emissions.}
}
\value{
List of magpie objects with results on country level, weight on country level, unit and description.
}
\description{
reads in the ACCMIP atmospheric deposition database. Speeds up caching
}
\examples{
\dontrun{
calcOutput("ACCMIP")
}
}
\seealso{
\code{\link[=calcAtmosphericDeposition]{calcAtmosphericDeposition()}}
}
\author{
Benjamin Leon Bodirsky
}
