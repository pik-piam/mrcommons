% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcNitrogenFixationPast.R
\name{calcNitrogenFixationPast}
\alias{calcNitrogenFixationPast}
\title{calcNitrogenFixationPast}
\usage{
calcNitrogenFixationPast(
  fixation_types = "both",
  sum_plantparts = TRUE,
  cellular = FALSE,
  irrigation = FALSE
)
}
\arguments{
\item{fixation_types}{either "fixation_crops", "fixation_freeliving", or "both"}

\item{sum_plantparts}{if false, crop residues, belowground residues and harvested organ are reported separately}

\item{cellular}{cellular estimates optional}

\item{irrigation}{if TRUE, distinguishes irrigated and non-irrigated crops}
}
\value{
List of magpie objects with results on country level, weight on country level, unit and description.
}
\description{
calculates fixation from freeliving bacteria and from nitrogen-fixing crops
}
\examples{
\dontrun{
calcOutput("calcNitrogenFixationPast")
}
}
\seealso{
\code{\link[=calcNitrogenFixationPast]{calcNitrogenFixationPast()}}
}
\author{
Benjamin Leon Bodirsky
}
