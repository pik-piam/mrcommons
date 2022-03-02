convertCEDS <- function(x, subtype) {

  # fill all missing countries with 0
  x[is.na(x)] <- 0

  x1 <- x["srb (kosovo)", , ]
  getItems(x1, dim = 1) <- "srb"
  x["srb", , ] <- x["srb", , ] + x1
  x          <- x[c("srb (kosovo)"), , , invert = TRUE]

  # Steve Smith 11.3.2016 on CEDS_Review_3-10-16.zip: there is a huge bug for fugitive emissions in zmb, and eth
  # I believe just past 2010. so do something to correct that (just keep those emissions constant from 2010 forward
  # for that sector in those two countries).

  # checked: all zero expept for NMVOC. But for NMVOC emission after 2010 do not
  # show significant deviations from before 2010

  # rename global to glo
  getItems(x, dim = 1) <- gsub("global", "glo", getItems(x, dim = 1))
  getItems(x, dim = 1) <- toupper(getItems(x, dim = 1))

  # most shipping and aviation data is global only (except 1A3dii_Domestic-navigation
  # regional). We want to distribute it evenly across all countries.
  # Therefore, save global data because it will be removed by toolCountryfill

  # 1A3dii_Domestic-navigation   regional (global value is zero )
  # 1A3di_International-shipping global   (no regional values exist)
  # 1A3ai_International-aviation global   (no regional values exist)
  # 1A3aii_Domestic-aviation     global   (no regional values exist)

  varGlob <- c("1A3di_International-shipping",
                "1A3ai_International-aviation",
                "1A3aii_Domestic-aviation")
  xGLO <- x["GLO", , varGlob]

  # remove global values. Note: the sector 2A1_Cement-production has a global
  # sum that is indentical to the sum over regions
  x <- x["GLO", , invert = TRUE]
  # fills missing ISO countires and remove unknown ISO countires
  x <- toolCountryFill(x, fill = 0)

  # Create weight 1 for xGLO
  w <- new.magpie(getItems(x, dim = 1), getItems(x, dim = 2), getItems(xGLO, dim = 3), fill = 1)

  # Create mapping of each country to GLO
  mapping <- data.frame(from = getItems(x, dim = 1), to = "GLO")

  # Spread global shipping and aviation data evenly across countries and save it to regions of x
  x[, , varGlob] <- toolAggregate(xGLO, mapping, weight = w)

  return(x)
}
