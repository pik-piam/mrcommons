#' @title calcFAOIntraYearProd

#' @description Distribute massbalanced or FAOSTAT staple production to monthly
#' or quarterly interval based on GGCMI crop calendar.
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
#' @importFrom withr local_options

calcFAOIntraYearProd <- function(day = "harvest_day", products = "kcr",
                                 frequency = "monthly", attribute = "dm") {

  local_options(magclass_sizeLimit = 1e+12)

  #### load GGCMI crop calendar and use rainfed dates for now

  cropcal <- readSource("GGCMICropCalendar", subtype = "cal",
                        convert = FALSE)[, , day][, , "rf"]

  ## aggregate wheat based on binary mask, note wheat mask is also already
  #masked to cropping area, maybe some inconsistencies when masking again to crop area
  wheat <-  readSource("GGCMICropCalendar", subtype = "wheat_areas", convert = FALSE)
  cropcal[, , "swh"] <- cropcal[, , "swh"] * wheat[, , "swh"]
  cropcal[, , "wwh"] <- cropcal[, , "wwh"] * wheat[, , "wwh"]
  cropcal <- add_columns(cropcal, addnm = "wheat", dim = 3.1, fill = 0)
  cropcal[, , "wheat"] <- cropcal[, , "swh"] + cropcal[, , "wwh"]
  cropcal <- cropcal[, , c("swh", "wwh"), invert = TRUE]

  ## mask cropcal to current area, assume 2010 area for now to avoid very large dataset
  areaMask <- calcOutput("Croparea", cellular = TRUE,
                         aggregate = FALSE)[, 2010, ]
  areaMask <- ifelse(areaMask > 0, 1, 0)
  areaMask <- areaMask[, , c("begr", "betr", "foddr", "oilpalm", "others"), invert = TRUE]

  ggcmiMapping <- toolGetMapping("MAgPIE_GGCMI.csv", type = "sectoral", where = "mappingfolder")
  areaMask <- toolAggregate(areaMask, rel = ggcmiMapping, from = "MagPIE", to = "GGCMI", dim = 3, partrel = TRUE)

  cropcal <- cropcal * setYears(areaMask, NULL)

  # make data frame to more easily count the share of cropping date within each country
  cropcaldf <-  as.data.frame(collapseNames(cropcal)) %>%
                group_by(.data$Value, .data$Region, .data$Data1) %>%
                dplyr::count() %>%
                filter(.data$Value != 0)

  cropcaldf <- cropcaldf %>%
               group_by(.data$Region, .data$Data1) %>%
               mutate("share" =  .data$n / sum(.data$n))

  names(cropcaldf)[names(cropcaldf) == "Value"] <- "day"
  cropcaldf <- cropcaldf[, c("Region", "day", "Data1", "share")]

  # convert back to magpie, fill missing countries
  cropcaldf <- as.magpie(cropcaldf, spatial = "Region", temporal = "day")
  cropcaldf <- toolCountryFill(cropcaldf, fill = 0)
  cropcaldf[is.na(cropcaldf)] <- 0

  daysMapping <- toolGetMapping("day_month_quarter.csv", type = "sectoral", where = "mappingfolder")

  if (frequency == "monthly") {
    cropcaldf <- toolAggregate(cropcaldf, rel = daysMapping, from = "day",
                               to = "month", dim = 2, partrel = TRUE)
  } else if (frequency == "quarterly") {
    cropcaldf <- toolAggregate(cropcaldf, rel = daysMapping, from = "day",
                               to = "quarter", dim = 2, partrel = TRUE)
  }

  # distribute rice 1 and 2 based on fraction of harvested area.
  # Distribute sorghum and millet to trce, and barley, wheat and
  # rye to tece based on average ratios of national FAO production
  # Distribute bean and pea to pulses
  rice <-  readSource("GGCMICropCalendar", subtype = "rice_areas", convert = FALSE)

  # aggregate rice to national harvested areas
  mapping <- toolGetMappingCoord2Country()
  mapping$coordiso <- paste(mapping$coords, mapping$iso, sep = ".")
  rice <- toolAggregate(rice, rel = mapping, from = "coordiso", to = "iso",
                         weight = new.magpie(cells_and_regions = getItems(rice, dim = 1), years = NULL,
                                            names = getNames(rice), fill = 1))
  rice <- toolCountryFill(rice, fill = 0)

  if (products == "kcr") {
    #### production
    prod <- collapseNames(calcOutput("Production", products = products,
                                     cellular = FALSE, attributes = "dm", aggregate = FALSE))



  if (frequency == "monthly") {
    iprod <- add_dimension(prod, dim = 2.2, add = "month", nm = c(unique(daysMapping$month)))
  } else if (frequency == "quarterly") {
    iprod <- add_dimension(prod, dim = 2.2, add = "quarter", nm = c(unique(daysMapping$quarter)))
  }

  iprod[, , "rice_pro"] <- prod[, , "rice_pro"] *
                   (setNames(rice[, , "ri1"] * cropcaldf[, , "ri1"], NULL) +
                      setNames(rice[, , "ri2"] * cropcaldf[, , "ri2"], NULL))

  tece <- c("44|Barley", "71|Rye", "15|Wheat")
  trce <- c("79|Millet", "83|Sorghum")
  puls <-  c("176|Beans, dry", "181|Broad beans, horse beans, dry",
            "187|Peas, dry", "197|Pigeon peas", "191|Chick peas")

  regionalProd <-  collapseNames(readSource("FAO_online", "Crop")[, getItems(iprod,
                                  dim = 2.1), "production"][, , c(tece, trce, puls)])

  teceRatio <- regionalProd[, , tece] /
                   dimSums(regionalProd[, , tece], dim = 3)
  teceRatio[is.na(teceRatio)] <- 0

  trceRatio <- regionalProd[, , trce] /
    dimSums(regionalProd[, , trce], dim = 3)
  trceRatio[is.na(trceRatio)] <- 0

  pulsRatio <- regionalProd[, , puls] /
    dimSums(regionalProd[, , puls], dim = 3)
  pulsRatio[is.na(pulsRatio)] <- 0


  iprod[, , "tece"] <- prod[, , "tece"] *
    (setNames(teceRatio[, , "44|Barley"], NULL) * setNames(cropcaldf[, , "bar"], NULL) +
    setNames(teceRatio[, , "71|Rye"], NULL) * setNames(cropcaldf[, , "rye"], NULL) +
    setNames(teceRatio[, , "15|Wheat"], NULL) * setNames(cropcaldf[, , "wheat"], NULL))

    iprod[, , "trce"] <- prod[, , "trce"] *
    (setNames(trceRatio[, , "79|Millet"], NULL) * setNames(cropcaldf[, , "mil"], NULL) +
       setNames(trceRatio[, , "83|Sorghum"], NULL) * setNames(cropcaldf[, , "sor"], NULL))

    iprod[, , "puls_pro"] <- prod[, , "puls_pro"] *
      (setNames(dimSums(pulsRatio[, , c("176|Beans, dry",
                                        "181|Broad beans, horse beans, dry")], dim = 3), NULL) *
         setNames(cropcaldf[, , "bea"], NULL) +
       setNames(dimSums(pulsRatio[, , c("187|Peas, dry",
                                        "197|Pigeon peas", "191|Chick peas")], dim = 3), NULL) *
                        setNames(cropcaldf[, , "pea"], NULL))

   productsLeft <- setdiff(findset("kcr"), c("rice_pro", "tece", "trce", "puls_pro"))
   calProductsLeft <- setdiff(getNames(cropcaldf), c("bar",
                                         "rye", "wheat", "ri1", "ri2", "mil", "sor", "pea", "bea"))
   cropcaldf <- toolAggregate(cropcaldf[, , calProductsLeft],
                              rel = ggcmiMapping, from = "GGCMI", to = "MagPIE", dim = 3, partrel = TRUE)

   missingProducts <- setdiff(productsLeft, getNames(cropcaldf))

   if (!is.null(missingProducts)) {
     cropcaldf <- add_columns(cropcaldf, addnm = missingProducts,
                              dim = 3.1, fill = 0)

         if (frequency == "monthly") {
            cropcaldf[, "September", missingProducts] <- 1
         } else if (frequency == "quarterly") {
           cropcaldf[, "q3", missingProducts] <- 1
    }
     vcat(1, "Missing calendar information for ", paste(missingProducts, " "),
          "These assumed (poorly) for now to have single harvest date in September/quarter 3")
   }

   iprod[, , productsLeft] <- prod[, , productsLeft] * cropcaldf[, , productsLeft]

  } else if (products == "staples") {
    staples <- c("56|Maize", "236|Soybeans", "15|Wheat",  "27|Rice, paddy")
    prod <-  collapseNames(readSource("FAO_online", "Crop")[, , staples][, , "production"])
    getNames(prod) <- c("maiz", "soybean", "wheat", "rice_pro")

    if (frequency == "monthly") {
      iprod <- add_dimension(prod, dim = 2.2, add = "month", nm = c(unique(daysMapping$month)))
    } else if (frequency == "quarterly") {
      iprod <- add_dimension(prod, dim = 2.2, add = "quarter", nm = c(unique(daysMapping$quarter)))
 }

    iprod[, , "rice_pro"] <- prod[, , "rice_pro"] *
      (setNames(rice[, , "ri1"] * cropcaldf[, , "ri1"], NULL) + setNames(rice[, , "ri2"] *
                                                                           cropcaldf[, , "ri2"], NULL))

    iprod[, , c("maiz", "soybean", "wheat")] <- prod[, , c("maiz", "soybean", "wheat")] *
                                 setNames(cropcaldf[, , c("mai", "soy", "wheat")],
                                          c("maiz", "soybean", "wheat"))

    ## convert to Mt
    iprod <- iprod / 10^6

    # convert to attribute
    prodAttributes <- collapseNames(calcOutput("Attributes", aggregate = FALSE)[, , attribute])
    getItems(prodAttributes, dim = 3)[getItems(prodAttributes, dim = 3) == "tece"] <- "wheat"

    iprod <- iprod * prodAttributes[, , getItems(iprod, dim = 3)]

} else {
  stop("Products so far can only kcr or staples")
  }
  out <- iprod

  return(list(x = out,
              weight = NULL,
              unit = "Mt DM/Nr/P/K/WM or PJ energy",
              description = "Crop production: dry matter: Mt (dm),
                             gross energy: PJ (ge), reactive nitrogen: Mt (nr),
                             phosphor: Mt (p), potash: Mt (k), wet matter: Mt (wm)."
              ))

  }
