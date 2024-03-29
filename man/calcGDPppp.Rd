% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcGDPppp.R
\name{calcGDPppp}
\alias{calcGDPppp}
\title{calcGDPppp}
\usage{
calcGDPppp(
  GDPpppCalib = NULL,
  GDPpppPast = NULL,
  GDPpppFuture = NULL,
  FiveYearSteps = TRUE,
  naming = NULL
)
}
\arguments{
\item{GDPpppCalib}{to what should be calibrated? past, future or a transition?}

\item{GDPpppPast}{GDPppp past data source}

\item{GDPpppFuture}{GDPppp future data source}

\item{FiveYearSteps}{Only five year steps if TRUE, FALSE returns years from source data}

\item{naming}{naming scheme}
}
\value{
GDP PPP(ICP11) in million USD05 equivalents
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

This function is defunct. Use \code{\link[mrdrivers:calcGDP]{mrdrivers::calcGDP()}} instead.
}
\seealso{
\code{\link[mrdrivers:calcGDP]{mrdrivers::calcGDP()}}
}
