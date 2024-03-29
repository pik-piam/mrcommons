% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcEmisNitrogenOceans.R
\name{calcEmisNitrogenOceans}
\alias{calcEmisNitrogenOceans}
\title{calcEmisNitrogenOceans}
\usage{
calcEmisNitrogenOceans(method = "ACCMIP")
}
\arguments{
\item{method}{deposition inventory}
}
\value{
List of magpie objects with results on country level, weight on country level, unit and description.
}
\description{
Conputes (nitrogen) emissions of Oceans.
}
\examples{
\dontrun{
calcOutput("EmisNitrogenOceans")
}

}
\seealso{
\code{\link[=calcAtmosphericDeposition]{calcAtmosphericDeposition()}},
\code{\link[=calcNitrogenBudgetCropland]{calcNitrogenBudgetCropland()}}
}
\author{
Benjamin Leon Bodirsky
}
