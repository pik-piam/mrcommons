#' Convert FAO data
#'
#' Converts FAO data to fit to the common country list and removes or converts
#' relative values where possible. Yields (Hg/ha) are for instance removed
#' since they can later easily be calculated from production and area but might
#' be problematic in the spatial aggregation. Per capita demand values are
#' transformed into absolute values using population estimates from the
#' calcPopulationPast function.
#'
#' Update 23-Jan-2017 - Added FAO Forestry production and trade data (Abhi)
#'
#' @param x MAgPIE object containing original values
#' @param subtype The FAO file type, e.g.: CBCrop
#' @return Data as MAgPIE object with common country list
#' @author Ulrich Kreidenweis, Abhijeet Mishra, Mishko Stevanovic, David Klein, Edna Molina Bacca
#' @seealso \code{\link{readFAO}}, \code{\link{readSource}},
#' @examples
#'
#' \dontrun{ a <- readSource("FAO","Crop", convert=TRUE)}
#' @importFrom magclass magpiesort
#'

## check why LivePrim has such strange Units such as (0_1Gr/An) and "Yield_(Hg)"

convertFAO_online <- function(x,subtype) {

  # ---- Settings ----

  ## datasets that have only absolute values
  absolute <- c("CBCrop", "CBLive", "CropProc", "Fertilizer", "Land", "LiveHead",
                "LiveProc", "Pop", "ValueOfProd","ForestProdTrade","Fbs")

  ## datasets that contain relative values that can be deleted because they can
  ## be calculated again at a later point in time
  ## and the dimensions that can be deleted
  relative_delete <- list()
  relative_delete[["Crop"]] <- c("Yield_(hg/ha)","Yield_(Hg/Ha)")
  relative_delete[["Fodder"]] <- "Yield_(Hg/Ha)"
  relative_delete[["LivePrim"]] <- c("Yield_Carcass_Weight_(Hg/An)",
                                     "Yield_(100Mg/An)",
                                     "Yield_Carcass_Weight_(0_1Gr/An)",
                                     "Yield_(Hg/An)",
                                     "Yield_(Hg)",
                                     "Yield_(100mg/An)",               # new FAO data
                                     "Yield_(hg/An)",                  # new FAO data
                                     "Yield_Carcass_Weight_(hg/An)",   # new FAO data
                                     "Yield_Carcass_Weight_(0_1g/An)", # new FAO data
                                     "Yield_(hg)")                     # new FAO data

  # Relative and unused datasets for the Capital Stock database
  relative_delete[["CapitalStock"]] <- c("22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Value_Local_Currency_(millions)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Value_Local_Currency_2015_prices_(millions)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Value_USD_(millions)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Share_of_Value_Added_Local_Currency_(percentage)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Share_of_Value_Added_Local_Currency_2015_prices_(percentage)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Share_of_Value_Added_USD_(percentage)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Agriculture_orientation_index_Local_Currency_(index)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Agriculture_orientation_index_Local_Currency_2015_prices_(index)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Agriculture_orientation_index_USD_(index)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Agriculture_orientation_index_USD_2015_prices_(index)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Share_of_Gross_Fixed_Capital_Formation_USD_(percentage)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Share_of_Gross_Fixed_Capital_Formation_(percentage)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Share_of_Gross_Fixed_Capital_Formation_2015_prices_(percentage)",
                                       "22031|Consumption of Fixed Capital (Agriculture, Forestry and Fishing).Value_Local_Currency_(millions)",
                                       "22031|Consumption of Fixed Capital (Agriculture, Forestry and Fishing).Value_Local_Currency_2015_prices_(millions)",
                                       "22031|Consumption of Fixed Capital (Agriculture, Forestry and Fishing).Value_USD_(millions)",
                                       "22034|Net Capital Stocks (Agriculture, Forestry and Fishing).Value_Local_Currency_(millions)",
                                       "22034|Net Capital Stocks (Agriculture, Forestry and Fishing).Value_Local_Currency_2015_prices_(millions)",
                                       "22034|Net Capital Stocks (Agriculture, Forestry and Fishing).Value_USD_(millions)",
                                       "22033|Gross Capital Stocks (Agriculture, Forestry and Fishing).Value_Local_Currency_(millions)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Share_of_Value_Added_USD_2015_prices_(percentage)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Share_of_Gross_Fixed_Capital_Formation_USD_2015_prices_(percentage)",
                                       "22030|Gross Fixed Capital Formation (Agriculture, Forestry and Fishing).Value_Local_Currency_(millions)")


  # select elements only if unit (dim=3.2) exists in x (otherwise magclass would complain when trying to remove non-existent elements with invert=TRUE). For capital stocks selects the complete name. The dot in the original dataset causes errors.
  relative_delete <- if((subtype %in% names(relative_delete)) & subtype != "CapitalStock") relative_delete[[subtype]][relative_delete[[subtype]] %in% getItems(x,dim=3.2)] else if(subtype == "CapitalStock") relative_delete[[subtype]][relative_delete[[subtype]] %in% getItems(x,dim=3)]  else NULL
  if (identical(relative_delete, character(0))) stop("For this subtype (",subtype,") units are listed in 'convertFAO' whose entries should be deleted from the data, but none of the specified units could be found in the data.")

  ## datasets that contain relative values: and define these dimensions
  relative <- list()
  relative[["FSCrop"]] <- c("food_supply_kg/cap/yr",
                            "food_supply_g/cap/day",
                            "food_supply_kcal/cap/day",
                            "protein_supply_g/cap/day",
                            "fat_supply_g/cap/day")

  relative[["FSLive"]] <- c("food_supply_kg/cap/yr",
                            "food_supply_g/cap/day",
                            "food_supply_kcal/cap/day",
                            "protein_supply_g/cap/day",
                            "fat_supply_g/cap/day")

  # ---- Section for country specific treatment ----

  ## data for Eritrea ERI added with 0 if not existing in the dimensionality of
  ## Ethiopia, to make toolISOhistorical work
  if(any(getRegions(x)=="XET") & any(getRegions(x)=="ETH") & !any(getRegions(x)=="ERI")) {
    xERI <- x["ETH",,]
    xERI[,,] <- 0
    getRegions(xERI) <- "ERI"
    x <- magpiesort(mbind(x,xERI))
  }

  ## add additional mappings
  additional_mapping <- list()

  # Eritrea ERI and Ethiopia ETH
  if (all(c("XET","ETH","ERI") %in% getRegions(x))) {
    additional_mapping <- append(additional_mapping, list(c("XET","ETH","y1992"),c("XET","ERI","y1992")))
  }

  # Belgium-Luxemburg
  if (all(c("XBL","BEL","LUX") %in% getRegions(x))) {
    additional_mapping <- append(additional_mapping, list(c("XBL","BEL","y1999"), c("XBL","LUX", "y1999")))
  } else if(("XBL" %in% getRegions(x)) & !("BEL" %in% getRegions(x))) {
    getRegions(x)[getRegions(x)=="XBL"] <- "BEL"
  }

  # Sudan (former) to Sudan and Southern Sudan. If non of the latter two is in the data make Sudan (former) to Sudan
  if (all(c("XSD", "SSD", "SDN") %in% getRegions(x))){
    additional_mapping <- append(additional_mapping, list(c("XSD","SSD","y2010"), c("XSD", "SDN","y2010")))
  } else if ("XSD" %in% getRegions(x) & !any(c("SDD", "SDN") %in% getRegions(x)) ) {
    getRegions(x)[getRegions(x) == "XSD"] <- "SDN"
  }

  ## if there is information for CHN (China) and XCN (China, mainland) and at least one of the regions
  ## HKG (China, Hong Kong SAR), TWN (China, Taiwan Province of), or MAC (China, Macao SAR)
  ## then replace CHN information by XCN, otherwise discard XCN
  if(any(getRegions(x)=="CHN") & any(getRegions(x)=="XCN") & any(getRegions(x) %in% c("HKG","TWN","MAC"))){
    China_mainland <- x["XCN",,]
    getRegions(China_mainland) <- "CHN"
    x["CHN",,] <- China_mainland
    x <- x["XCN",,,invert=T]
  } else if (any(getRegions(x) == "XCN") & subtype == "CapitalStock") { #turns XCN into CHN
    China_mainland <- x["XCN",,]
    getRegions(China_mainland) <- "CHN"
    x <- mbind(x,China_mainland)
    x <- x["XCN",,,invert=T]
  }else if (any(getRegions(x) == "XCN") & subtype != "CapitalStock") {
    x <- x["XCN",,,invert=T]
  }

  ## data for the Netherlands Antilles is currently removed because currently no
  ## information for its successors SXM, CUW, ABW is available as input for toolISOhistorical
  if(any(getRegions(x) == "ANT")) {
    x <- x["ANT",,,invert=T]
  }

  ## data for PCI split up into:
  # Marshall Islands (MH, MHL, 584)
  # Micronesia, Federated States of (FM, FSM, 583)
  # Northern Mariana Islands (MP, MNP, 580)
  # Palau (PW, PLW, 585)
  if (all(c("PCI", "MHL", "FSM", "MNP", "PLW") %in% getRegions(x))){
    additional_mapping <- append(additional_mapping, list(c("PCI","MHL","y1991"), c("PCI", "FSM","y1991"), c("PCI", "MNP","y1991"), c("PCI", "PLW","y1991")))
  } else if ("PCI" %in% getRegions(x)) {
    x <- x["PCI",,invert=T]
  }


  ### For certain subtypes: if some of the follow up states of the Soviet Union (SUN), Yugoslavia (YUG), Serbia and Montenegro (SCG) are missing add them with values of 0
  if(subtype %in% c("EmisAgRiceCult","Fertilizer","EmisAgCultOrgSoil","EmisLuCrop","EmisLuGrass","EmisAgSynthFerti")) {
    ISOhistorical <- read.csv2(system.file("extdata","ISOhistorical.csv",package = "madrat"),stringsAsFactors = F)
    former <- ISOhistorical[ISOhistorical$fromISO %in% c("SUN", "YUG", "SCG"),"toISO"]
    missing <- former[!former %in% getRegions(x)]
    x2 <- new.magpie(cells_and_regions = missing, years=getYears(x), names = getNames(x))
    x2[,getYears(x2)[getYears(x2, as.integer = T)>=1992],] <- 0
    x <- mbind(x,x2)
  }

  # ---- Treatment of absolute or relative values ----

  if (any(subtype == absolute)) {
    x[is.na(x)] <- 0
    x <- toolISOhistorical(x, overwrite = TRUE, additional_mapping = additional_mapping)
    x <- toolCountryFill(x, fill = 0, verbosity = 2)
    if (any(grepl(pattern = 'yield|Yield|/', getNames(x, fulldim=T)[[2]]))) warning("The following elements could be relative: \n", paste(grep(pattern = 'yield|Yield|/', getNames(x, fulldim=T)[[2]], value=TRUE),collapse=" "), "\n" , "and would need a different treatment of NAs in convertFAO")

  } else if (!is.null(relative_delete)) {
    x[is.na(x)] <- 0
    x <- x[,,relative_delete, invert=T]
    x <- if (subtype != "CapitalStock") toolISOhistorical(x, overwrite = TRUE, additional_mapping = additional_mapping) else x #Capital Stock available starting from 1995 (no need for transitions)
    x <- toolCountryFill(x, fill=0, verbosity = 2)
    if(subtype != "CapitalStock") if (any(grepl(pattern = 'yield|Yield|/', getNames(x, fulldim=T)[[2]]))) warning("The following elements could be relative: \n", paste(grep(pattern = 'yield|Yield|/', getNames(x, fulldim=T)[[2]], value=TRUE),collapse=" "), "\n" , "and would need a different treatment of NAs in convertFAO")

  } else if (any(subtype == c("FSCrop", "FSLive"))) {

    xabs <- x[,,relative[[subtype]], invert=T]
    xrel <- x[,,relative[[subtype]], invert=F]

    # handling of relative values
    # replaced toolISOhistorical by the following approach for disaggregation
    mapping <- read.csv2(system.file("extdata","ISOhistorical.csv",package = "madrat"),stringsAsFactors = F)
    for(elem in additional_mapping) {    mapping <- rbind(mapping,elem)  }

    .adopt_aggregated_average <- function(country,data,mapping){
      if(length(country)>1){stop("only one transition per function call")}
      toISO=mapping$toISO[mapping$fromISO==country]
      lastyear=unique(mapping$lastYear[mapping$fromISO==country])
      if (length(lastyear)>1){stop("strange transition mapping")}
      allyears = getYears(data,as.integer = T)
      years = allyears[allyears <= as.integer(substring(lastyear,2,5))]
      data[toISO,years,] = magclass::colSums(data[country,years])
      data <- data[country,,,invert=T]
      return(data)
    }

    xrel=.adopt_aggregated_average(country = "SUN",data=xrel,mapping = mapping)
    xrel=.adopt_aggregated_average(country = "YUG",data=xrel,mapping = mapping)
    xrel=.adopt_aggregated_average(country = "CSK",data=xrel,mapping = mapping)
    xrel=.adopt_aggregated_average(country = "XET",data=xrel,mapping = mapping)
    xrel=.adopt_aggregated_average(country = "XBL",data=xrel,mapping = mapping)
    xrel=.adopt_aggregated_average(country = "SCG",data=xrel,mapping = mapping)
    xrel=.adopt_aggregated_average(country = "XSD",data=xrel,mapping = mapping)

    # transforming relative values into absolute values
    pop <- calcOutput("PopulationPast",aggregate=FALSE)
    xrel <- toolCountryFill(xrel, fill=0, verbosity = 2)
    commonyears <- intersect(getYears(pop), getYears(x))
    xrelpop <- collapseNames(complete_magpie(pop[,commonyears,])*complete_magpie(xrel[,commonyears,]))
    xrelpop <- xrelpop[,,c("food_supply_kcal/cap/day","protein_supply_g/cap/day","fat_supply_g/cap/day")] *365
    getNames(xrelpop,dim = 2) <- c("food_supply_kcal","protein_supply","fat_supply")
    xrelpop[is.na(xrelpop)] <- 0

    # absolute values
    xabs[is.na(xabs)]=0
    xabs[xabs<0]=0
    xabs <- toolISOhistorical(xabs, overwrite = TRUE, additional_mapping = additional_mapping)
    xabs <- toolCountryFill(xabs, fill=0, verbosity = 2)

    x <- mbind(xabs, xrelpop)
    x <- complete_magpie(x)
    x <- toolCountryFill(x, fill=0, verbosity = 2)
    if (any(grepl(pattern = 'yield|Yield|/', getNames(x, fulldim=T)[[2]]))) warning("The following elements could be relative: \n", paste(grep(pattern = 'yield|Yield|/', getNames(x, fulldim=T)[[2]], value=TRUE),collapse=" "), "\n" , "and would need a different treatment of NAs in convertFAO")

  # automatically delete the "Implied emissions factor XXX" dimension for Emission datasets
  } else if (substring(subtype,1,6)=="EmisAg" | substring(subtype,1,6)=="EmisLu") {
    if (any(grepl("Implied_emission_factor", fulldim(x)[[2]][[4]]))) {
      x <- x[,,"Implied_emission_factor", pmatch=T, invert=T]
    }
     x[is.na(x)] <- 0
     x <- toolISOhistorical(x, overwrite = TRUE, additional_mapping = additional_mapping)
     x <- toolCountryFill(x, fill=0, verbosity = 2)

  # Producer Prices Annual
  } else if(subtype %in% c("PricesProducerAnnual","PricesProducerAnnualLCU")){
    # FAO changed the unit. Look for all possible names and select only existing ones from the magpie object
    possible_names <- list (PricesProducerAnnual    = c("Producer_Price_(US_$_tonne)_(USD)","Producer_Price_(USD_tonne)_(USD)"),
                            PricesProducerAnnualLCU = c("Producer_Price_(Standard_local_Currency_tonne)_(SLC)","Producer_Price_(SLC_tonne)_(SLC)"))
    possible_names <- toolSubtypeSelect(subtype,possible_names)
    x <- collapseNames(x[,,possible_names[possible_names %in% getItems(x,dim=3.2)]])
    ## Serbia and Montenegro split
    if(all(c("SCG","SRB") %in% getRegions(x)) & !"MNE" %in% getRegions(x)){
      mne <- x["SRB",,]
      dimnames(mne)[[1]] <- "MNE"
      x <- mbind(x, mne)
    }
    ## Adjust prices of live animal weight to the carcass weight
    mapping <- toolGetMapping("FAO_livestock_carcass_price_factor.csv",type="sectoral",where="mrcommons")
    for(item in mapping$FAO_carcass){
      litem <- mapping$FAO_live_weigth[grep(item, mapping$FAO_carcass)]
      countries <- getRegions(which(!is.na(x[,,item]),arr.ind=TRUE))
      countries <- setdiff(getRegions(x), countries)
      x[countries,,item] <- x[countries,,litem]/mapping$Price_factor[grep(item, mapping$FAO_carcass)]
    }
    x[is.na(x)] <- 0
    x <- toolISOhistorical(x, overwrite=TRUE, additional_mapping=additional_mapping)
    x <- toolCountryFill(x, fill=0, verbosity=2)

  } else {
    cat("Specify in convertFAO whether dataset contains absolute or relative values!")
  }

  # ---- Set negative values to 0 (except stock variation) ----

  if(length(fulldim(x)[[2]])>3){
    novar <- setdiff(fulldim(x)[[2]][[4]], "stock_variation")
    x[,,novar][x[,,novar]<0] <- 0
  }

  return(x)
}
