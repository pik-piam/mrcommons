
#' Read MAgPIE data
#' 
#' Read-in MAgPIE data
#' 
#' @param subtype Either "EmiAirPoll", "macBase" or "co2tax"
#' @importFrom madrat regionscode
#' @return magpie object
#' @author Julian Oeser
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="MAgPIE",subtype="EmiAPExo")
#' }

readMAgPIE <- function(subtype) {
  
  if (subtype == "EmiAirPoll") {
    x <- read.csv("emiAPexo.csv", row.names = 1)
    # reorder the data frame
    x$var   <- x$value
    x$value <- NULL
    # convert into a magpie object
    x <- as.magpie(x, datacol=6)
    
  } else if (subtype == "macBase") {
    x <- read.csv("macBaseMagpie.csv", row.names = 1)
    # reorder the data frame
    x$var   <- x$value
    x$value <- NULL
    # convert into a magpie object
    x <- as.magpie(x, datacol=6)
    
  } else if (subtype == "macBaseCO2luc") {
    x <- read.magpie("p_macBaseMagpie_co2luc_SSP2.cs4r")

  } else if (subtype == "co2tax") {
    x <- read.magpie("p_magpietax200.cs4r")

  } else if (subtype == "abatparam_co2") {
    x <- read.magpie("p_abatparam_CO2.cs4r")
    
  } else if (subtype == "MAgPIEReport_extensive") {
    
    # last version before the current /p/tmp/aloisdir/magpie/output
    # current version /p/projects/piam/runs/coupled-magpie/output
    
    # !!! ATTENTION !!! 
    # Please update scenario names in calcMAgPIEReport.R 

    file_list <- c("C_SDP-Base-mag-4.mif",
                   "C_SDP-PkBudg900-mag-4.mif",
                   "C_SDP-PkBudg1300-mag-4.mif",
                   "C_SDP-NDC-mag-4.mif",
                   "C_SSP1-Base-mag-4.mif",
                   "C_SSP1-PkBudg900-mag-4.mif",
                   "C_SSP1-PkBudg1300-mag-4.mif",
                   "C_SSP1-NDC-mag-4.mif",
                   "C_SSP2-Base-mag-4.mif",
                   "C_SSP2-PkBudg900-mag-4.mif",
                   "C_SSP2-PkBudg1300-mag-4.mif",
                   "C_SSP2-NDC-mag-4.mif",
                   "C_SSP5-Base-mag-4.mif",
                   "C_SSP5-PkBudg900-mag-4.mif",
                   "C_SSP5-PkBudg1300-mag-4.mif",
                   "C_SSP5-NDC-mag-4.mif")
    
    x <- NULL
    for(f in file_list) {
      x <- mbind(x,read.report(f,as.list = FALSE))
    }

  } else if (subtype == "supplyCurve_magpie_40") {
    regcode <- regionscode(toolGetMapping(type = "regional", name = getConfig("regionmapping"), returnPathOnly = TRUE))
    
    # !!! ATTENTION !!! 
    # Please update scenario names in calcBiomassPrice.R if necessary
    
    scenario_names <- c("f30_bioen_price_SDP-NDC-NDC_replaced_flat_",
                        "f30_bioen_price_SDP-NDC-PkBudg1300_replaced_flat_",
                        "f30_bioen_price_SDP-NDC-PkBudg900_replaced_flat_",
                        "f30_bioen_price_SDP-NPI-Base_replaced_flat_",
                        "f30_bioen_price_SSP1-NDC-NDC_",
                        "f30_bioen_price_SSP1-NDC-PkBudg1300_replaced_flat_",
                        "f30_bioen_price_SSP1-NDC-PkBudg900_",
                        "f30_bioen_price_SSP1-NPI-Base_",
                        "f30_bioen_price_SSP2-NDC-NDC_",
                        "f30_bioen_price_SSP2-NDC-PkBudg1300_",
                        "f30_bioen_price_SSP2-NDC-PkBudg900_replaced_flat_",
                        "f30_bioen_price_SSP2-NPI-Base_",
                        "f30_bioen_price_SSP5-NDC-NDC_replaced_flat_",
                        "f30_bioen_price_SSP5-NDC-PkBudg1300_replaced_flat_",
                        "f30_bioen_price_SSP5-NDC-PkBudg900_replaced_flat_",
                        "f30_bioen_price_SSP5-NPI-Base_replaced_flat_")
    

    file_list <- paste0(scenario_names,regcode,".cs4r")
    setnames  <- c("region","year","scenario","char")
    
    if(!all(file.exists(file_list))) {
      vcat(1,"Could not find ",file_list[!file.exists(file_list)],"\n")
      vcat(1,"MAgPIE supplycurve input is not available for all policy cases for the regioncode",regcode,".\nUsing fallback input files.\n")
      # if emulators with current regional resolution are not available for ALL scenarios, use fallback files
      regcode_H12 <- "690d3718e151be1b450b394c1064b1c5"   # has to be updated based on the new regioncodehash
      file_list   <- paste0(scenario_names,regcode_H12,".cs4r")
      setnames    <- c("region_fallback","year","scenario","char")
    }

    x <- NULL
    for(f in file_list) {
      x <- mbind(x,read.magpie(f))
    }
    getSets(x) <- setnames
    
    # make SSP2Ariadne scenario using SSP2 data --- ATTENTION: needs to be deleted as soon as we have data for SSP2Ariadne
    x_SSP2Ariadne <- x[,,c("SSP2-NDC-NDC","SSP2-NDC-PkBudg1300","SSP2-NDC-PkBudg900","SSP2-NPI-Base")]
    getNames(x_SSP2Ariadne) <- gsub("SSP2", "SSP2Ariadne", getNames(x_SSP2Ariadne))
    x <- mbind(x, x_SSP2Ariadne)
    # make SDP* scenarios using SSP1 data --- ATTENTION: needs to be deleted as soon as we have data for SDP*
    x_SDP_EI <- x[,,c("SSP1-NDC-NDC","SSP1-NDC-PkBudg1300","SSP1-NDC-PkBudg900","SSP1-NPI-Base")]
    getNames(x_SDP_EI) <- gsub("SSP1", "SDP_EI", getNames(x_SDP_EI))
    x_SDP_RC <- x[,,c("SSP1-NDC-NDC","SSP1-NDC-PkBudg1300","SSP1-NDC-PkBudg900","SSP1-NPI-Base")]
    getNames(x_SDP_RC) <- gsub("SSP1", "SDP_RC", getNames(x_SDP_RC))
    x_SDP_MC <- x[,,c("SSP1-NDC-NDC","SSP1-NDC-PkBudg1300","SSP1-NDC-PkBudg900","SSP1-NPI-Base")]
    getNames(x_SDP_MC) <- gsub("SSP1", "SDP_MC", getNames(x_SDP_MC))
    x <- mbind(x, x_SDP_EI, x_SDP_RC, x_SDP_MC)

  } else {
    stop("Not a valid subtype!")
  }
  return(x)
}

