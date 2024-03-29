% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcRegressionParameters.R
\name{calcRegressionParameters}
\alias{calcRegressionParameters}
\title{calcRegressionParameters}
\usage{
calcRegressionParameters(regression = "bmi_shr")
}
\arguments{
\item{regression}{bmi_shr: Shares of BMI withina population. schofield or FAO_WHO_UNU1985:
calculates intake based on anthropometrics}
}
\value{
List of magpie objects with results on country level, weight on country level, unit and description.
}
\description{
Writes down the equation parameters from various data sources
}
\examples{
\dontrun{
calcOutput("RegressionParameters")
}

}
\author{
Benjamin Leon Bodirsky
}
