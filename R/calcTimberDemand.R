#' @title calcTimberDemand
#' @description
#' Calculates the demand of timber from FAO data (including intermediate products).
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @seealso
#' [mrfaocore::calcFAOmassbalance_pre()]
#' @examples
#' \dontrun{
#' calcOutput("TimberDemand")
#' }
#' @importFrom magclass getNames<- as.magpie
#' @export

calcTimberDemand <- function() {
  x <- readSource("FAO_online", "ForestProdTrade")

  ## Remove distinction between coniferous and non-coniferous part
  x <- x[, , sort(getNames(x)[grep(pattern = "coniferous", x = getNames(x), invert = TRUE)])]

  ## Remove strange numbers (FAO item codes?)
  itemCodeItem <- "ItemCodeItem"
  getNames(x, dim = itemCodeItem) <- gsub(
    pattern = "^[0-9]+\\|",
    replacement = "",
    x = getNames(x, dim = itemCodeItem)
  )

  elementShort <- "ElementShort"

  ## Remove monetary data, we are only interested in m3 or mio. ton data
  x <- x[, , grep(pattern = "kUS\\$", x = getNames(x, dim = elementShort), invert = TRUE, value = TRUE)]
  x <- x[, , sort(getNames(x))]

  ## FAO separated "Particle board and OSB" into "Particle board" and "OSB"
  ## Why? Who knows
  ## Merging back into "Particle board and OSB"
  x[, , "Particle board and OSB (1961-1994)"] <- (x[, , "Particle board and OSB (1961-1994)"]
                                                  + x[, , "Particle board"]
                                                  + x[, , "OSB"])

  ## drop "Particle Board" and "OSB"
  x <- x[, , c("Particle board", "OSB"), invert = TRUE]

  ## Rename "Particle board and OSB (1961-1994)" to "Particle board and OSB"
  getNames(x, dim = itemCodeItem) <- gsub(pattern = "Particle board and OSB \\(1961-1994\\)",
                                          replacement = "Particle board and OSB",
                                          x = getNames(x, dim = itemCodeItem))

  ## Pulpwood category is strangely split in data because it'll be a miracle if FAO could stick to consistent naming
  ## we now merge this in one
  pulpwoodNames <- grep(pattern = "Pulpwood", x = getNames(x, dim = 1), value = TRUE)

  ## isolate all pulpwood data
  pulpwoodData <- x[, , pulpwoodNames]

  ## Repitition of export, import data and unnecessary split has been made in three categories
  ## "Pulpwood and particles (1961-1997).export_m3" and "Pulpwood, round and split,
  ## all species (export/import, 1961-1989).export_m3"
  ## report the same data. We can just use "Pulpwood and particles (1961-1997).export_m3"
  pulpwoodExport <- pulpwoodData[, , "Pulpwood and particles (1961-1997).export_m3"]
  pulpwoodImport <- pulpwoodData[, , "Pulpwood and particles (1961-1997).import_m3"]
  ## Merge production data
  pulpwoodDescription <- "Pulpwood, round and split, all species (production).production"
  pulpwoodProduction <- (pulpwoodData[, , "Pulpwood and particles (1961-1997).production"]
                         + setNames(pulpwoodData[, , pulpwoodDescription], NULL))

  ## Corrected pulpwood data
  pulpwood <- mbind(pulpwoodProduction, pulpwoodExport, pulpwoodImport)
  getNames(pulpwood, dim = 1) <- "Pulpwood"

  ## Pulpwood export and import data is not available after 1989 (from 1990)
  lastShareExport <- collapseNames(pulpwood[, 1989, "export_m3"] / pulpwood[, 1989, "production"])
  lastShareExport[is.na(lastShareExport)] <- 0
  lastShareExport[lastShareExport > 1] <- 1 ## COG exports more than production??

  lastShareImport <- collapseNames(pulpwood[, 1989, "import_m3"] / pulpwood[, 1989, "production"])
  lastShareImport[is.na(lastShareImport)] <- 0
  lastShareImport[lastShareImport > 1] <- 1

  pulpwood[, 1990:2019, "export_m3"] <- pulpwood[, 1990:2019, "production"] * lastShareExport
  pulpwood[, 1990:2019, "import_m3"] <- pulpwood[, 1990:2019, "production"] * lastShareImport
  ## Pulpwood export import data is not reported

  ## remove pulpwood data fom raw data momentarily
  x <- x[, , pulpwoodNames, invert = TRUE]

  ## Add back pulpwood
  x <- mbind(x, pulpwood)

  # Sort naming
  x <- x[, , sort(getNames(x, dim = 1))]

  ## Extract variables we need and only choose variables which which have data for all three categories:
  ## Production in m3
  ## Import in m3
  ## Export in m3
  ## Here, wood pulp is an exception which is in mio. tonnes so we will assume 450 kg/m3 density there

  variablesNeeded <- c(
    "Roundwood",
    "Industrial roundwood", "Wood fuel",
    "Other industrial roundwood", "Pulpwood",
    "Sawlogs and veneer logs", "Fibreboard", "Particle board and OSB",
    "Wood pulp", "Sawnwood", "Plywood", "Veneer sheets", "Wood-based panels"
  )

  if (length(variablesNeeded[!(variablesNeeded %in% getNames(x, dim = 1))]) > 0) {
    message("Category '", variablesNeeded[!(variablesNeeded %in% getNames(x, dim = 1))],
            "' does not exist in source FAO data. Manual correction to code will be needed.")
  }
  ## in above variables, remeber that Sawlogs and veneer logs need an additional category called
  ## "SLVL based wood" which we will calculate back based on the sum of Sawnwood, plywood, venner sheets,
  ## wood residues and wood chips.
  ## Also remeber that Pulpwood round and split only has data for production and not import and export so
  ## that has to be calculated back based on its constituents.
  ## See https://i.imgur.com/VackFiv.png for better clarity.

  timberFao <- x[, , variablesNeeded] ### Remember that wood pulp is in mio tonnes so we have to convert it to m3

  ## We'll consider the production, import and export separately

  production <- timberFao[, , grep(pattern = "production",
                                   x = getNames(timberFao, dim = elementShort), value = TRUE)]
  import <- timberFao[, , grep(pattern = "import", x = getNames(timberFao, dim = elementShort), value = TRUE)]
  export <- timberFao[, , grep(pattern = "export", x = getNames(timberFao, dim = elementShort), value = TRUE)]

  ## Next step is to convert wood pulp from t to m3

  production[, , "Wood pulp"] <- production[, , "Wood pulp"] * 1000 / 450 ## 10^3 for t to kg. 450 for kg to m3
  import[, , "Wood pulp"] <- import[, , "Wood pulp"] * 1000 / 450 ## 10^3 for t to kg. 450 for kg to m3
  export[, , "Wood pulp"] <- export[, , "Wood pulp"] * 1000 / 450 ## 10^3 for t to kg. 450 for kg to m3

  ## Remove all units as now everything is converted to m3
  getNames(production, dim = elementShort) <- gsub(pattern = "_m3",
                                                   replacement = "",
                                                   x = getNames(production, dim = elementShort))
  getNames(import, dim = elementShort) <- gsub(pattern = "_m3",
                                               replacement = "",
                                               x = getNames(import, dim = elementShort))
  getNames(export, dim = elementShort) <- gsub(pattern = "_m3",
                                               replacement = "",
                                               x = getNames(export, dim = elementShort))
  ## in the import and export objects, we need to add segment for pulpwood which is the sum of fibreboard,
  ## particle board OSB and Wood pulp

  toAdd <- setdiff(getNames(production, dim = 1), getNames(export, dim = 1))

  import <- add_columns(x = import, addnm = toAdd, dim = 3.1)
  export <- add_columns(x = export, addnm = toAdd, dim = 3.1)

  import[, , toAdd] <- 0 ## No import and export data for pulpwood from FAO
  export[, , toAdd] <- 0 ## No import and export data for pulpwood from FAO

  ## Another category to add here is "Other Sawnwood" (refer to ??)
  toAdd <- "Other sawnwood"
  production <- add_columns(x = production, addnm = toAdd, dim = 3.1)
  production[, , toAdd] <- (production[, , "Sawlogs and veneer logs"]
                            - dimSums(production[, , c("Plywood", "Veneer sheets", "Sawnwood")], dim = 3))
  if (min(production[, , toAdd]) < 0) {
    message("Negative values detected when adding category '", toAdd, "'. Setting negative values to 0.")
  }
  production[, , toAdd][production[, , toAdd] < 0] <- 0
  import <- add_columns(x = import, addnm = toAdd, dim = 3.1)
  export <- add_columns(x = export, addnm = toAdd, dim = 3.1)
  import[, , toAdd] <- 0 ## No import and export data for Other sawnwood from FAO
  export[, , toAdd] <- 0 ## No import and export data for Other sawnwood from FAO

  ###### Correction Stage
  ###### Refer to http://dx.doi.org/10.1016/j.rser.2016.09.107
  ###### Paper titled "The wood from the trees: The use of timber in construction"
  indicatorList <- c()
  for (indicator in getNames(production, dim = itemCodeItem)) {
    incorrectData <- where(production[, , indicator][, , "production"] < export[, , indicator][, , "export"])$true
    if (length(unique(incorrectData$regions)) > 0) {
      indicatorList <- c(indicatorList, indicator)
      export[incorrectData$individual[, 1], incorrectData$individual[, 2], indicator][, , "export"] <- (
        production[incorrectData$individual[, 1], incorrectData$individual[, 2], indicator][, , "production"]) # nolint
    }
  }
  message("Higher exports than production level detected in some countries. Setting these export values ",
          "to production value. Following categories affected: ", paste0(indicatorList, ", "))

  ## Now we are ready to merge back production, import and export
  timberFaoCleaned <- mbind(production, import, export)

  ###### Data cleaning stage
  timberFaoCleaned <- add_columns(timberFaoCleaned, addnm = "other_util", dim = 3.2)
  timberFaoCleaned[, , "other_util"] <- (timberFaoCleaned[, , "production"]
                                         + timberFaoCleaned[, , "import"]
                                         - timberFaoCleaned[, , "export"])

  timberFaoCleaned <- add_columns(timberFaoCleaned, addnm = "domestic_supply", dim = 3.2)
  timberFaoCleaned[, , "domestic_supply"] <- timberFaoCleaned[, , "other_util"]

  ## Now convert data to mio. m3
  timberFaoCleaned <- timberFaoCleaned / 1e6

  ## Check if negatvive values still exist
  if (min(timberFaoCleaned) < 0) {
    message("CAUTION: Negative values still exist in cleaned FAO timber production-import-export data.")
    timberFaoCleaned[timberFaoCleaned < 0] <- 0
    message("Such values are set to 0 now but could be problematic. Check R Script which processes this data")
  }

  out <- timberFaoCleaned

  return(list(
    x = out,
    weight = NULL,
    min = 0,
    unit = "mio m3",
    description = "Calculates the timber demand based on historical FAO data"
  ))
}
