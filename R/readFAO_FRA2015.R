#' Read FAO_FRA2015
#'
#' Read-in an FRA data from 2015 (forest resource assessment)
#'
#'
#' @param subtype data subtype. Either "production" or "fac" (forest area and characteristics)
#' or "biodiversity" or "anndat" (Annual Data)
#' @return magpie object of the FRA 2015 data
#' @author Abhijeet Mishra
#' @seealso [readSource()]
#' @examples
#' \dontrun{
#' a <- readSource("FAO_FRA2015", "production")
#' }
#'
#' @importFrom magclass as.magpie
#' @importFrom madrat toolSubtypeSelect
#' @importFrom tools file_ext
#' @importFrom data.table fread
#' @importFrom utils unzip
#' @importFrom tools file_path_sans_ext
#' @export


readFAO_FRA2015 <- function(subtype) { # nolint

  is.nan.data.frame <- function(x) { # nolint
    do.call(cbind, lapply(x, is.nan))
  }

  if (subtype == "production") {
    csvtoread <- "2.PRODUCTION.csv"
  }
  if (subtype == "fac") {
    csvtoread <- "1.FOREST AREA AND CHARACTERISTICS.csv"
  }
  if (subtype == "biodiversity") {
    csvtoread <- "5.BIODIVERSITY AND CONSERVATION.csv"
  }
  if (subtype == "anndat") {
    csvtoread <- "9. ANNUAL DATA.csv"
  } # Notice the sapce in name. That's how it is in data received from FRA2015

  if (subtype == "fac") {
    # read the data
    data <- read.csv(unz("BULK.zip", csvtoread), header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)

    # Subset the data by removig the year "9999". No eoutplanation in FRA 2015 reoprt.
    data <- subset(data, data$Year != 9999)

    # Keep only the variables we need
    data <- data[, c("Country", "Year", "Forest", "Forchange", "ForPerc", "PerCapFor", "NatFor",
                     "Nfchange", "OthWooLan", "OthLan", "LanTreCov", "InWater", "Landarea", "ForExp",
                     "Afforest", "NatForExp", "Deforest", "HumDef", "Reforest", "ArtRef", "PrimFor",
                     "NatRegFor", "IntroSpp", "NatzedSpp", "PlantFor", "Pfchange", "IntroSppPlant",
                     "Mangrove")]

    # More data cleaning
    data$Forchange <- suppressWarnings(as.numeric(data$Forchange)) # nolint
    data$Forchange[which(is.na(data$Forchange))] <- 0
    data$Landarea <- as.numeric(gsub(",", "", data$Landarea)) # nolint

    # Make country as a "char"
    data$Country <- as.character(data$Country) # nolint

    ## Now we want to make sure that the mapping we have can be applied to the dataset in hand
    mapping <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")

    # See how many countries are available in the mapping file
    mapIso <- sort(unique(mapping$iso))

    # find out which countries are missing from our dataset of FRA 2015 (as compared to mapping countries)
    absent <- setdiff(mapIso, unique(data$Country))                    # Missing countries to add
    yeardiff <- subset(unique(data$Year), !is.na(unique(data$Year)))    # Missing years to add

    # Add empty rows to data frame which contains missing countries from our dataset
    # create a one-row matrix the same length as data
    temprow <- matrix(c(rep.int(NA, length(data))),
                      nrow = length(yeardiff),
                      ncol = length(data))

    # make it a data.frame and give cols the same names as data
    newrow <- data.frame(temprow)
    colnames(newrow) <- colnames(data)
    rm(temprow)

    # Add these new dataset for each new country to original dataset
    for (i in seq_along(absent)) {
      for (j in seq_along(yeardiff)) {
        newrow[j, "Year"] <- yeardiff[j]
      }
      newrow[, "Country"] <- absent[i]
      data <- rbind(data, newrow)
    }

    # Subset to countries which are in our updated dataset as compared to all
    # countries for which ISO code is available in the mapping file
    for (i in seq_along(mapIso)) {
      for (j in seq_len(nrow(data))) {
        if (data[j, "Country"] == mapIso[i]) {
          data[j, "logic"] <- 1
        }
      }
    }
    data <- subset(data, data$logic == 1)
    data <- data[, -ncol(data)]

    # Now we deal with NAs
    for (i in which(vapply(data, is.numeric, logical(1)))) {
      for (j in which(is.na(data[, i]))) {
        data[j, i] <- mean(data[data[, "Country"] == data[j, "Country"], i],  na.rm = TRUE)
      }
    }

    data[is.nan(data)] <- 0
    data <- as.magpie(data)
    return(data)
  } else if (subtype == "production") {
    # read the data
    data <- read.csv(unz("BULK.zip", csvtoread), header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)

    # Subset the data by removig the year "9999". No eoutplanation in FRA 2015 reoprt.
    data <- subset(data, data$Year != 9999)

    # Keep only the variables we need
    data <- data[, c("Country", "Year", "ForGrow", "ConifGrow", "BroaGrow", "OthWooGrow", "OthConifGrow",
                     "OthBroaGrow", "NetAnnIncr", "IncrConif", "IncrBroa", "ForAbovCarb", "OthAbovCarb",
                     "ForBelCarb", "OthBelCarb", "ForSubLiv", "OthSubLiv", "ForSoilCarb", "OthSoilCarb",
                     "ProdFor", "MulUseFor")]

    # Make country as a "char"
    data$Country <- as.character(data$Country) # nolint

    ## Now we want to make sure that the mapping we have can be applied to the dataset in hand
    mapping <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")

    # See how many countries are available in the mapping file
    mapIso <- sort(unique(mapping$iso))

    # find out which countries are missing from our dataset of FRA 2015 (as compared to mapping countries)
    absent <- setdiff(mapIso, unique(data$Country))                    # Missing countries to add
    yeardiff <- subset(unique(data$Year), !is.na(unique(data$Year)))    # Missing years to add

    # Add empty rows to data frame which contains missing countries from our dataset
    # create a one-row matrix the same length as data
    temprow <- matrix(c(rep.int(NA, length(data))),
                      nrow = length(yeardiff),
                      ncol = length(data))

    # make it a data.frame and give cols the same names as data
    newrow <- data.frame(temprow)
    colnames(newrow) <- colnames(data)
    rm(temprow)

    # Add these new dataset for each new country to original dataset
    for (i in seq_along(absent)) {
      for (j in seq_along(yeardiff)) {
        newrow[j, "Year"] <- yeardiff[j]
      }
      newrow[, "Country"] <- absent[i]
      data <- rbind(data, newrow)
    }

    # Subset to countries which are in our updated dataset as compared to all
    # countries for which ISO code is available in the mapping file
    for (i in seq_along(mapIso)) {
      for (j in seq_len(nrow(data))) {
        if (data[j, "Country"] == mapIso[i]) {
          data[j, "logic"] <- 1
        }
      }
    }
    data <- subset(data, data$logic == 1)
    data <- data[, -ncol(data)]

    # Now we deal with NAs
    for (i in which(vapply(data, is.numeric, logical(1)))) {
      for (j in which(is.na(data[, i]))) {
        data[j, i] <- mean(data[data[, "Country"] == data[j, "Country"], i],  na.rm = TRUE)
      }
    }

    data[is.nan(data)] <- 0
    data <- as.magpie(data)
    return(data)
  } else if (subtype == "biodiversity") {
    # read the data
    data <- read.csv(unz("BULK.zip", csvtoread), header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)

    # Subset the data by removig the year "9999". No eoutplanation in FRA 2015 reoprt.
    data <- subset(data, data$Year != 9999)

    # Keep only the variables we need
    data <- data[, c("Country", "Year", "BioCons", "ProtArea")]

    # Make country as a "char"
    data$Country <- as.character(data$Country) # nolint

    ## Now we want to make sure that the mapping we have can be applied to the dataset in hand
    mapping <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")

    # See how many countries are available in the mapping file
    mapIso <- sort(unique(mapping$iso))

    # find out which countries are missing from our dataset of FRA 2015 (as compared to mapping countries)
    absent <- setdiff(mapIso, unique(data$Country))                    # Missing countries to add
    yeardiff <- subset(unique(data$Year), !is.na(unique(data$Year)))    # Missing years to add

    # Add empty rows to data frame which contains missing countries from our dataset
    # create a one-row matrix the same length as data
    temprow <- matrix(c(rep.int(NA, length(data))),
                      nrow = length(yeardiff),
                      ncol = length(data))

    # make it a data.frame and give cols the same names as data
    newrow <- data.frame(temprow)
    colnames(newrow) <- colnames(data)
    rm(temprow)

    # Add these new dataset for each new country to original dataset
    for (i in seq_along(absent)) {
      for (j in seq_along(yeardiff)) {
        newrow[j, "Year"] <- yeardiff[j]
      }
      newrow[, "Country"] <- absent[i]
      data <- rbind(data, newrow)
    }

    # Subset to countries which are in our updated dataset as compared to all
    # countries for which ISO code is available in the mapping file
    for (i in seq_along(mapIso)) {
      for (j in seq_len(nrow(data))) {
        if (data[j, "Country"] == mapIso[i]) {
          data[j, "logic"] <- 1
        }
      }
    }
    data <- subset(data, data$logic == 1)
    data <- data[, -ncol(data)]

    # Now we deal with NAs
    for (i in which(vapply(data, is.numeric, logical(1)))) {
      for (j in which(is.na(data[, i]))) {
        data[j, i] <- mean(data[data[, "Country"] == data[j, "Country"], i],  na.rm = TRUE)
      }
    }

    data[is.nan(data)] <- 0
    data <- as.magpie(data)
    return(data)
  } else if (subtype == "anndat") {
    # read the data
    # Notice that the separator in this particular csv file is ; (not ,) and decimal is , (not .)
    data <- read.csv(unz("BULK.zip", csvtoread), header = TRUE, sep = ";",
                     dec = ",", stringsAsFactors = FALSE)

    # Subset the data by removig the year "9999". No eoutplanation in FRA 2015 reoprt.
    data <- subset(data, data$Year != 9999)

    # Keep only the variables we need
    data <- data[, c("Country", "Year", "WooRemov", "WooFuel")]

    # Make country as a "char"
    data$Country <- as.character(data$Country) # nolint

    ## Now we want to make sure that the mapping we have can be applied to the dataset in hand
    mapping <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mrcommons")

    # See how many countries are available in the mapping file
    mapIso <- sort(unique(mapping$iso))

    # find out which countries are missing from our dataset of FRA 2015 (as compared to mapping countries)
    absent <- setdiff(mapIso, unique(data$Country))                    # Missing countries to add
    yeardiff <- subset(unique(data$Year), !is.na(unique(data$Year)))    # Missing years to add

    # Add empty rows to data frame which contains missing countries from our dataset
    # create a one-row matrix the same length as data
    temprow <- matrix(c(rep.int(NA, length(data))),
                      nrow = length(yeardiff),
                      ncol = length(data))

    # make it a data.frame and give cols the same names as data
    newrow <- data.frame(temprow)
    colnames(newrow) <- colnames(data)
    rm(temprow)

    # Add these new dataset for each new country to original dataset
    for (i in seq_along(absent)) {
      for (j in seq_along(yeardiff)) {
        newrow[j, "Year"] <- yeardiff[j]
      }
      newrow[, "Country"] <- absent[i]
      data <- rbind(data, newrow)
    }

    # Subset to countries which are in our updated dataset as compared to all
    # countries for which ISO code is available in the mapping file
    for (i in seq_along(mapIso)) {
      for (j in seq_len(nrow(data))) {
        if (data[j, "Country"] == mapIso[i]) {
          data[j, "logic"] <- 1
        }
      }
    }
    data <- subset(data, data$logic == 1)
    data <- data[, -ncol(data)]

    # Now we deal with NAs
    for (i in which(vapply(data, is.numeric, logical(1)))) {
      for (j in which(is.na(data[, i]))) {
        data[j, i] <- mean(data[data[, "Country"] == data[j, "Country"], i],  na.rm = TRUE)
      }
    }

    data[is.nan(data)] <- 0
    data <- as.magpie(data)
    data <- add_columns(data, addnm = "WooRW", dim = 3.1)
    data[, , "WooRW"] <- data[, , "WooRemov"] - data[, , "WooFuel"]
    return(data)
  } else {
    stop("Invalid subtype ", subtype)
  }
}
