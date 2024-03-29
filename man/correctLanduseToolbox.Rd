% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correctLanduseToolbox.R
\name{correctLanduseToolbox}
\alias{correctLanduseToolbox}
\title{correctLanduseToolbox}
\usage{
correctLanduseToolbox(x)
}
\arguments{
\item{x}{magpie object provided by the read function}
}
\value{
corrected magpie object
}
\description{
correct Landuse Toolbox output data. Convert unit from ha to mio ha
}
\examples{
\dontrun{
A <- readSource("LanduseToolbox", convert = "onlycorrect")
}

}
\seealso{
\code{\link{readLanduseToolbox}}
}
\author{
David Hoetten
}
