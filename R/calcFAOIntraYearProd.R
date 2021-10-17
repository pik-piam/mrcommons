#' @title calcFAOIntraYearProd

#' @description Distribute massbalanced or FAOSTAT staple production to monthly or quarterly interval based on GGCMI crop calendar.
#' Only national level implemented for now as cellular production only available on 5 year time steps due to memory.
#' Assume rainfed crop calendar date for now.
  
#' @param day harvest_day (to market) or maturity_day (first mature)
#' @param frequency monthly or quarterly. Daily leads to memory limits. 
#' @param products "kcr" or "staples" staples uses FAO production dataset instead of calcProduction
#' to only give maize wheat soy and rice. Allows for more years. A bit of a David-specific subtype 
#' @param attribute dm default. can only select one at a time due to memory
#' 
#' #' @seealso
#' \code{\link{readGGCMICropCalendar}}
#' 
#' @author David Chen
#' 
#' @importFrom dplyr group_by count %>% mutate filter
#' @importFrom magpiesets findset

calcFAOIntraYearProd <- function(day = "harvest_day", products="kcr", frequency="monthly", attribute="dm"){
# 
   sizelimit <- getOption("magclass_sizeLimit")
   options(magclass_sizeLimit = 1e+12)
   on.exit(options(magclass_sizeLimit = sizelimit))

  #### load GGCMI crop calendar and use rainfed dates for now

  cropcal <- readSource("GGCMICropCalendar", subtype="cal", convert=F)[,,day][,,"rf"]

  ## aggregate wheat based on binary mask, note wheat mask is also already masked to cropping area, maybe some inconsistencies when masking again to crop area
  wheat <-  readSource("GGCMICropCalendar", subtype="wheat_areas", convert=F)
  cropcal[,,"swh"] <- cropcal[,,"swh"] * wheat[,,"swh"]
  cropcal[,,"wwh"] <- cropcal[,,"wwh"] * wheat[,,"wwh"]
  cropcal <- add_columns(cropcal, addnm="wheat", dim=3.1, fill=0)
  cropcal[,,"wheat"] <- cropcal[,,"swh"] + cropcal[,,"wwh"]
  cropcal <- cropcal[,,c("swh","wwh"), inv=T]
 
  ## mask cropcal to current area, assume 2010 area for now to avoid very large dataset
  area_mask <- calcOutput("Croparea", cellular=T, aggregate=F)[,2010,]
  area_mask <- ifelse(area_mask > 0, 1, 0)
  area_mask <- area_mask[,,c("begr","betr", "foddr", "oilpalm", "others"), inv=T]

  ggcmi_mapping <- toolGetMapping("MAgPIE_GGCMI.csv", type="sectoral")
  area_mask <- toolAggregate(area_mask, rel=ggcmi_mapping, from="MagPIE", to="GGCMI", dim=3, partrel=T)
    
  cropcal <- cropcal * setYears(area_mask, NULL)
 
  # make data frame to more easily count the share of cropping date within each country
  cropcaldf <-  as.data.frame(collapseNames(cropcal)) %>% 
  group_by(.data$Value, .data$Region, .data$Data1) %>%  dplyr::count() %>% 
  filter(.data$Value != 0)
  
  cropcaldf <- cropcaldf %>% group_by(.data$Region, .data$Data1) %>% 
                mutate("share" =  .data$n/sum(.data$n)) 
  
  names(cropcaldf)[names(cropcaldf)=="Value"] <- "day"
  cropcaldf <- cropcaldf[,c("Region","day", "Data1", "share")]
  
  #convert back to magpie, fill missing countries
  cropcaldf <- as.magpie(cropcaldf, spatial = "Region", temporal = "day" )
  cropcaldf <- toolCountryFill(cropcaldf, fill=0) 
  cropcaldf[is.na(cropcaldf)] <- 0
  
  days_mapping <- toolGetMapping("day_month_quarter.csv", type="sectoral")
  
  if(frequency=="monthly") {
  cropcaldf <- toolAggregate(cropcaldf, rel=days_mapping, from="day", to="month", dim=2, partrel=T)  
  } else if (frequency == "quarterly") {
    cropcaldf <- toolAggregate(cropcaldf, rel=days_mapping, from="day", to="quarter", dim=2, partrel=T)  
    
  }
 
  ## distribute rice 1 and 2 based on fraction of harvested area. 
  ## Distribute sorghum and millet to trce, and barley, wheat and rye to tece based on average ratios of national FAO production
  ## Distribute bean and pea to pulses 
  rice <-  readSource("GGCMICropCalendar", subtype="rice_areas", convert=F)
  #aggregate rice to national harvested areas
  
  mapping <- toolGetMapping(type="cell", name="CountryToCellMapping.csv")
  
  rice <- toolAggregate(rice,rel=mapping, from ="celliso", to ="iso", 
                         weight= new.magpie(cells_and_regions = getItems(rice,dim=1),years = NULL,
                                            names=getNames(rice), fill=1))
  rice <- toolCountryFill(rice, fill=0)
  
  
if (products == "kcr") {
    #### production
    prod <- collapseNames(calcOutput("Production", products=products, 
                                     cellular=FALSE, attributes="dm", aggregate = F))
  
  
  
  if(frequency=="monthly") {
  iprod <- add_dimension(prod, dim=2.2, add = "month", nm = c(unique(days_mapping$month))) 
  } else if (frequency == "quarterly"){
    iprod <- add_dimension(prod, dim=2.2, add = "quarter", nm = c(unique(days_mapping$quarter))) }
  
  iprod[,,"rice_pro"] <- prod[,,"rice_pro"] * 
                   (setNames(rice[,,"ri1"] * cropcaldf[,,"ri1"],NULL) + setNames(rice[,,"ri2"] * cropcaldf[,,"ri2"],NULL))  
  
  tece <- c("44|Barley", "71|Rye", "15|Wheat")
  trce <- c("79|Millet", "83|Sorghum")
  puls <-  c("176|Beans, dry", "181|Broad beans, horse beans, dry",
            "187|Peas, dry","197|Pigeon peas","191|Chick peas")
  
  regional_prod <-  collapseNames(readSource("FAO_online", "Crop")[,getItems(iprod,dim=2.1),"production"][,,c(tece,trce,puls)])

  tece_ratio <- regional_prod[,,tece]/
                   dimSums(regional_prod[,,tece],dim=3)
  tece_ratio[is.na(tece_ratio)] <- 0
  
  trce_ratio <- regional_prod[,,trce]/
    dimSums(regional_prod[,,trce],dim=3)
  trce_ratio[is.na(trce_ratio)] <- 0
  
  puls_ratio <- regional_prod[,,puls]/
    dimSums(regional_prod[,,puls],dim=3)
  puls_ratio[is.na(puls_ratio)] <- 0
  
  
  iprod[,,"tece"] <- prod[,,"tece"] * 
    (setNames(tece_ratio[,,"44|Barley"],NULL) * setNames(cropcaldf[,,"bar"],NULL) +
    setNames(tece_ratio[,, "71|Rye"],NULL) * setNames(cropcaldf[,,"rye"],NULL) +
    setNames(tece_ratio[,, "15|Wheat"],NULL) * setNames(cropcaldf[,,"wheat"],NULL))
  
    iprod[,,"trce"] <- prod[,,"trce"] * 
    (setNames(trce_ratio[,,"79|Millet"],NULL) * setNames(cropcaldf[,,"mil"],NULL) +
       setNames(trce_ratio[,, "83|Sorghum"],NULL) * setNames(cropcaldf[,,"sor"],NULL))
  
    iprod[,,"puls_pro"] <- prod[,,"puls_pro"] * 
      (setNames(dimSums(puls_ratio[,,c("176|Beans, dry", "181|Broad beans, horse beans, dry")],dim=3),NULL) * setNames(cropcaldf[,,"bea"],NULL) +
       setNames(dimSums(puls_ratio[,,c("187|Peas, dry","197|Pigeon peas","191|Chick peas")],dim=3),NULL) * setNames(cropcaldf[,,"pea"],NULL))
    
   products_left <- setdiff(findset("kcr"), c("rice_pro", "tece","trce","puls_pro"))
   cal_products_left <- setdiff(getNames(cropcaldf), c("bar","rye","wheat", "ri1","ri2","mil","sor","pea","bea"))
   cropcaldf <- toolAggregate(cropcaldf[,,cal_products_left], rel=ggcmi_mapping, from="GGCMI", to="MagPIE", dim=3, partrel=T)
    
   missing_products <- setdiff(products_left, getNames(cropcaldf))
   
   if (!is.null(missing_products)){
     cropcaldf <- add_columns(cropcaldf, addnm=missing_products, dim = 3.1, fill = 0)

         if (frequency == "monthly") {
            cropcaldf[,"September",missing_products] <- 1
         } else if (frequency == "quarterly") {
           cropcaldf[,"q3",missing_products] <- 1
    }
     vcat(1,"Missing calendar information for ", paste(missing_products, " "), "These assumed (poorly) for now to have single harvest date in September/quarter 3")
   }

   iprod[,,products_left] <- prod[,,products_left] * cropcaldf[,,products_left]
       
  
  }  
  
else if (products == "staples") {
    staples <- c("56|Maize", "236|Soybeans", "15|Wheat",  "27|Rice, paddy")
    prod <-  collapseNames(readSource("FAO_online", "Crop")[,,staples][,,"production"])
    getNames(prod) <- c("maiz", "soybean", "wheat", "rice_pro")
    
    
    
    if(frequency=="monthly") {
      iprod <- add_dimension(prod, dim=2.2, add = "month", nm = c(unique(days_mapping$month))) 
    } else if (frequency == "quarterly"){
      iprod <- add_dimension(prod, dim=2.2, add = "quarter", nm = c(unique(days_mapping$quarter))) }
    
    iprod[,,"rice_pro"] <- prod[,,"rice_pro"] * 
      (setNames(rice[,,"ri1"] * cropcaldf[,,"ri1"],NULL) + setNames(rice[,,"ri2"] * cropcaldf[,,"ri2"],NULL))  
    
    iprod[,,c("maiz","soybean","wheat")] <- prod[,,c("maiz","soybean","wheat")] *
                                 setNames(cropcaldf[,,c("mai","soy","wheat")], c("maiz","soybean","wheat"))
    
    ##convert to Mt 
    iprod <- iprod/10^6
    
    #convert to attribute
    ProdAttributes <- collapseNames(calcOutput("Attributes", aggregate = FALSE)[,,attribute])
    getNames(ProdAttributes[,,"tece"]) <- "wheat"
   
    iprod <- iprod * ProdAttributes[,,getItems(iprod, dim=3)]  
  
}
  
else stop("Products so far can only kcr or staples")

  out <- iprod
  
  return(list(x = out,
              weight = NULL,
              unit = "Mt DM/Nr/P/K/WM or PJ energy",
              description = "Crop production: dry matter: Mt (dm), gross energy: PJ (ge), reactive nitrogen: Mt (nr), phosphor: Mt (p), potash: Mt (k), wet matter: Mt (wm)."
              ))
  
  }
