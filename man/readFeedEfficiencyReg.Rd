% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readFeedEfficiencyReg.R
\name{readFeedEfficiencyReg}
\alias{readFeedEfficiencyReg}
\title{Read in regression coefficients for feed efficiency}
\usage{
readFeedEfficiencyReg()
}
\value{
MAgPIE object containing regression coefficients
}
\description{
Read in csv file containing coefficients of non-linear regression analysis
for the calculation of future feed efficiencies of feed baskets dependent
on livestock productivity trends
}
\examples{
\dontrun{
a <- readSource("FeedEfficiencyReg")
}
}
\seealso{
\code{\link[madrat:readSource]{madrat::readSource()}}
}
\author{
Isabelle Weindl
}
