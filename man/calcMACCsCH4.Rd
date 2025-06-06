% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcMACCsCH4.R
\name{calcMACCsCH4}
\alias{calcMACCsCH4}
\title{Calculation of CH4 MAC curves of Energy Industry and Landuse}
\usage{
calcMACCsCH4(sector = "all", source = "ImageMacc")
}
\arguments{
\item{sector}{"all" or "landuse"; "all" includes energy_industry and landuse}

\item{source}{"ImageMacc" or "PBL_MACC_2019"}
}
\value{
MAgPIE object
}
\description{
Calculation of the CH4 relative costcurves (subtypes: Energy Industry and
Landuse) weighted by the baseline emissions. Sources: CH4 coal
losses/leakages, CH4 oil losses/leakages, CH4 natural gas losses/leakages,
CH4 Landfills, CH4 Domestic Sewage, CH4 Wetland rice, CH4 Animals, CH4
Animal waste divided in classes 1-201.
}
\examples{
\dontrun{
calcOutput("MACCsCH4")
}
}
\seealso{
\code{\link[madrat:calcOutput]{madrat::calcOutput()}}, \code{\link[=readImageMacc]{readImageMacc()}},
\code{\link[=convertImageMacc]{convertImageMacc()}}
}
\author{
Nele Steinmetz, Florian Humpenoeder, Michael Windisch
}
