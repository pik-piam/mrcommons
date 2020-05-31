#' Calculation of CH4 MAC curves of Energy Industry and Landuse
#' 
#' Calculation of the CH4 relative costcurves (subtypes: Energy Industry and
#' Landuse) weighted by the baseline emissions. Sources: CH4 coal
#' losses/leakages, CH4 oil losses/leakages, CH4 natural gas losses/leakages,
#' CH4 Landfills, CH4 Domestic Sewage, CH4 Wetland rice, CH4 Animals, CH4
#' Animal waste divided in classes 1-201.
#' 
#' 
#' @param sector "all" or "landuse"; "all" includes energy_industry and landuse
#' @param source "ImageMacc" or "PBL_MACC_2019"
#' @return MAgPIE object
#' @author Nele Steinmetz, Florian Humpenoeder
#' @seealso \code{\link{calcOutput}}, \code{\link{readImageMacc}},
#' \code{\link{convertImageMacc}}
#' @examples
#' 
#' \dontrun{ 
#' calcOutput("MACCsCH4")
#' 
#' }
#' @importFrom magclass getNames
calcMACCsCH4 <- function(sector="all",source="ImageMacc") {
  
  # readSource CH4 and baseline Emissions
  if(source == "ImageMacc") {
    
    unit <- "Tax level 200 steps each 5$/tC"
    description <- "CH4 ImageMacc"
    
    energy_ind <- readSource("ImageMacc", "CH4_Energy_Industry")
    landuse <- readSource("ImageMacc", "CH4_Landuse")
    
    CH4 <- mbind(energy_ind, landuse)
    
#    rem_years <- sort(c(getYears(CH4,as.integer = TRUE),seq(2015,2055,10)))
    
    CH4 <- time_interpolate(CH4, seq(2015,2095,10), integrate_interpolated_years=TRUE, extrapolation_type="linear")
    
    getNames(CH4) <- gsub("CH4 coal losses/leakages"       ,"ch4coal",getNames(CH4))
    getNames(CH4) <- gsub("CH4 oil losses/leakages"        ,"ch4oil",getNames(CH4))
    getNames(CH4) <- gsub("CH4 natural gas losses/leakages","ch4gas",getNames(CH4))
    getNames(CH4) <- gsub("CH4 Landfills"                  ,"ch4wstl",getNames(CH4))
    getNames(CH4) <- gsub("CH4 Domestic Sewage"            ,"ch4wsts",getNames(CH4))
    getNames(CH4) <- gsub("CH4 Wetland rice"               ,"ch4rice",getNames(CH4))
    getNames(CH4) <- gsub("CH4 Animals"                    ,"ch4animals",getNames(CH4))
    getNames(CH4) <- gsub("CH4 Animal waste"               ,"ch4anmlwst",getNames(CH4))
    
    # weight for the aggregation
    baseline <- readSource("ImageMacc", "baseline_sources")
    w <- baseline[,getYears(CH4),c("CH4 coal losses/leakages", "CH4 oil losses/leakages", 
                                   "CH4 natural gas losses/leakages", "CH4 Landfills", 
                                   "CH4 Domestic Sewage", "CH4 Wetland rice", "CH4 Animals", 
                                   "CH4 Animal waste")]
    
    getNames(w) <- gsub("CH4 coal losses/leakages"       ,"ch4coal",getNames(w))
    getNames(w) <- gsub("CH4 oil losses/leakages"        ,"ch4oil",getNames(w))
    getNames(w) <- gsub("CH4 natural gas losses/leakages","ch4gas",getNames(w))
    getNames(w) <- gsub("CH4 Landfills"                  ,"ch4wstl",getNames(w))
    getNames(w) <- gsub("CH4 Domestic Sewage"            ,"ch4wsts",getNames(w))
    getNames(w) <- gsub("CH4 Wetland rice"               ,"ch4rice",getNames(w))
    getNames(w) <- gsub("CH4 Animals"                    ,"ch4animals",getNames(w))
    getNames(w) <- gsub("CH4 Animal waste"               ,"ch4anmlwst",getNames(w))
    
  } else if (source == "PBL_MACC_2019") {
    
    unit <- "Tax level 200 steps each 20$/tC"
    description <- "CH4 PBL_MACC_2019"

    wanted_years <- seq(2010,2100,by=5)
    
    CH4 <- NULL
    for (subtype in c("ch4coal","ch4oil","ch4gas","ch4wstl","ch4wsts","ch4rice","ch4animals","ch4anmlwst")) {
      x <- readSource("PBL_MACC_2019", subtype)
      existing_years <- getYears(x,as.integer = T)
      tmp <- setdiff(wanted_years,existing_years)
      missing_years <- tmp[tmp<existing_years[1]]
      x <- x[,intersect(wanted_years,existing_years),]
      x <- toolFillYears(x,c(missing_years,getYears(x,as.integer = T)))
      y <- time_interpolate(x, wanted_years, integrate_interpolated_years=TRUE, extrapolation_type="linear")
      names(dimnames(y)) <- names(dimnames(x))
      CH4 <- mbind(CH4,y)
    }
    
    # weight for the aggregation
    baseline <- readSource("PBL_MACC_2019", "baseline_sources")
    w <- baseline[,getYears(CH4),getNames(CH4,dim=1)]
    
  }
  
  w[w==0] <- 1e-10 #asigning a very small number to countries with zero emissions so if regions that are resulting from zero emission country aggergations still have a value associated 
  
  if (sector == "all") {
    rem_years <- c(seq(2010,2055,by=5),seq(2060,2100,by=10))
    CH4 <- CH4[,rem_years,]
    w <- w[,rem_years,]
  } else if(sector == "landuse") {
    CH4 <- CH4[,,c("ch4rice","ch4animals","ch4anmlwst")]
    getNames(CH4) <- gsub("ch4rice","rice_ch4",getNames(CH4))
    getNames(CH4) <- gsub("ch4animals","ent_ferm_ch4",getNames(CH4))
    getNames(CH4) <- gsub("ch4anmlwst","awms_ch4",getNames(CH4))
    x <- new.magpie(getRegions(CH4),seq(2105,2150,5),getNames(CH4),0)
    x[,,] <- setYears(CH4[,2100,],NULL)
    CH4 <- mbind2(CH4,x)
    
    w <- w[,,c("ch4rice","ch4animals","ch4anmlwst")]
    getNames(w) <- gsub("ch4rice","rice_ch4",getNames(w))
    getNames(w) <- gsub("ch4animals","ent_ferm_ch4",getNames(w))
    getNames(w) <- gsub("ch4anmlwst","awms_ch4",getNames(w))
    x <- new.magpie(getRegions(w),seq(2105,2150,5),getNames(w),0)
    x[,,] <- setYears(w[,2100,],NULL)
    w <- mbind2(w,x)
  }
  
  return(list(x=CH4,weight=w,unit=unit,description=description))
}
