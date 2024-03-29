% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcNutrientBudgetSewage.R
\name{calcNutrientBudgetSewage}
\alias{calcNutrientBudgetSewage}
\title{calcNutrientBudgetSewage}
\usage{
calcNutrientBudgetSewage(historic = TRUE)
}
\arguments{
\item{historic}{when TRUE only for the historic period, otherwise including future scenarios}
}
\value{
List of magpie objects with results on country level, weight on country level, unit and description.
}
\description{
Nutrient Budget for Wastewater treatment and sewage
}
\examples{
\dontrun{
calcOutput("NutrientBudgetSewage")
}


}
\seealso{
\code{\link[=calcNitrogenBudgetCropland]{calcNitrogenBudgetCropland()}}
}
\author{
Benjamin Leon Bodirsky
}
