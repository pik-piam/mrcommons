#' @title readEDGARfood
#' @description read Edgar-food v6 food system emission data
#' @param subtype Type of data that should be read
#' \itemize{
#' \item \code{foodSystemEmi}: Total food system emissions of different countries
#' \item \code{foodSystemShare}: Share of food system emissions in total emissions
#' \item \code{foodSystemSector}: Food system emissions separated by country, sector and substance
#' }
#' @return A magpie object with foodsystem total emissions,
#' emission shares or sector and substance specific emission, depending on subtype
#' @author David Hötten
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' @importFrom  stats xtabs
#'
readEDGARfood <- function(subtype) {

  if (subtype == "foodSystemEmi") {
    sheetEmi <- readxl::read_excel("EDGAR-FOOD_v6.xlsx", sheet = 2, skip = 3)
    sheetEmi$Name <- NULL
    names(sheetEmi) <- gsub("Y_", "y", names(sheetEmi))
    magpieEmi <- as.magpie(sheetEmi, spatial = 1, temporal = 2)
    getSets(magpieEmi) <- c("Country_code_A3", "years", "data")
    out <-   magpieEmi
  } else if (subtype == "foodSystemShare") {
    sheetShare <- readxl::read_excel("EDGAR-FOOD_v6.xlsx", sheet = 3, skip = 3)
    names(sheetShare)[-1] <- paste0("y", names(sheetShare)[-1])
    magpieShare <- as.magpie(sheetShare, spatial = 1, temporal = 2)
    getSets(magpieShare) <- c("Country_code_A3", "years", "data")
    out <-   magpieShare
  } else if (subtype == "foodSystemSector") {
    sheetSector <- read_excel("EDGAR-FOOD_v6.xlsx",
                              sheet = 4,
                              skip = 3, na = "NULL",
                              col_types = c(rep("text", 6), rep("numeric", 29)))
    sheetSectorLong <- pivot_longer(sheetSector,
                                    names(sheetSector)[7:35],
                                    names_to = "year",
                                    values_to = "value")
    sheetSectorLong$year <- gsub("Y_", "y", sheetSectorLong$year)
    sheetSectorArray <- xtabs(value ~ Country_code_A3 + year + Substance + FOOD_system_stage,
                               sheetSectorLong)
    class(sheetSectorArray) <- "array"
    magpieSector <- as.magpie(sheetSectorArray, spatial = 1, temporal = 2)
    out <- magpieSector
  } else {
    stop("Invalid subtype argument")
  }

  return(out)

}
