% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcNitrogenBNF.R
\name{calcNitrogenBNF}
\alias{calcNitrogenBNF}
\title{calcNitrogenBNF}
\usage{
calcNitrogenBNF(cellular = FALSE)
}
\arguments{
\item{cellular}{cellular disaggreagation or national values}
}
\value{
List of magpie objects with results on country level, weight on country level, unit and description.
}
\description{
calculates fixation from freeliving bacteria and from nitrogen-fixing crops and natural vegetation
}
\examples{
\dontrun{
calcOutput("NitrogenBNF")
}

}
\seealso{
\code{\link[=calcNitrogenFixationPast]{calcNitrogenFixationPast()}}
}
\author{
Benjamin Leon Bodirsky
}
