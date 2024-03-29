% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correctIPCCClimate.R
\name{correctIPCCClimate}
\alias{correctIPCCClimate}
\title{correctIPCCClimate}
\usage{
correctIPCCClimate(x)
}
\arguments{
\item{x}{magpie object provided by the read function}
}
\value{
Magpie object with results on cellular level for 12 IPCC climate zone types
}
\description{
Correct IPCC climate classification
}
\examples{
\dontrun{
readSource("IPCCClimate", convert = "onlycorrect")
}

}
\author{
Kristine Karstens
}
