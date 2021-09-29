#' @title calcTimberDemand
#' @description
#' Calculates the demand of timber from FAO data (including intermediate products).
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Abhijeet Mishra
#' @seealso
#' \code{\link{calcFAOmassbalance_pre}}
#' @examples
#'
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
  getNames(x, dim = "ItemCodeItem") <- gsub(
    pattern = "^[0-9]+\\|",
    replacement = "",
    x = getNames(x, dim = "ItemCodeItem")
  )

  ## Remove monetary data, we are only interested in m3 or mio. ton data
  x <- x[, , grep(pattern = "kUS\\$", x = getNames(x, dim = "ElementShort"), invert = TRUE, value = TRUE)]
  x <- x[, , sort(getNames(x))]

  ## FAO separated "Particle board and OSB" into "Particle board" and "OSB"
  ## Why? Who knows
  ## Merging back into "Particle board and OSB"
  x[, , "Particle board and OSB (1961-1994)"] <- x[, , "Particle board and OSB (1961-1994)"] + x[, , "Particle board"] + x[, , "OSB"]

  ## drop "Particle Board" and "OSB"
  x <- x[, , c("Particle board", "OSB"), invert = TRUE]

  ## Rename "Particle board and OSB (1961-1994)" to "Particle board and OSB"
  getNames(x, dim = "ItemCodeItem") <- gsub(pattern = "Particle board and OSB \\(1961-1994\\)", replacement = "Particle board and OSB", x = getNames(x, dim = "ItemCodeItem"))

  ## Pulpwood category is strangely split in data because it'll be a miracle if FAO could stick to consistent naming
  ## we now merge this in one
  pulpwood_names <- grep(pattern = "Pulpwood", x = getNames(x, dim = 1), value = TRUE)

  ## isolate all pulpwood data
  pulpwood_data <- x[, , pulpwood_names]

  ## Repitition of export, import data and unnecessary split has been made in three categories
  ## "Pulpwood and particles (1961-1997).export_m3" and "Pulpwood, round and split, all species (export/import, 1961-1989).export_m3"
  ## report the same data. We can just use "Pulpwood and particles (1961-1997).export_m3"
  pulpwood_export <- pulpwood_data[, , "Pulpwood and particles (1961-1997).export_m3"]
  pulpwood_import <- pulpwood_data[, , "Pulpwood and particles (1961-1997).import_m3"]
  ## Merge production data
  pulpwood_production <- pulpwood_data[, , "Pulpwood and particles (1961-1997).production"] + setNames(pulpwood_data[, , "Pulpwood, round and split, all species (production).production"], NULL)

  ## Corrected pulpwood data
  pulpwood <- mbind(pulpwood_production, pulpwood_export, pulpwood_import)
  getNames(pulpwood, dim = 1) <- "Pulpwood"

  ## Pulpwood export and import data is not available after 1989 (from 1990)
  last_share_export <- collapseNames(pulpwood[, 1989, "export_m3"] / pulpwood[, 1989, "production"])
  last_share_export[is.na(last_share_export)] <- 0
  last_share_export[last_share_export > 1] <- 1 ## COG exports more than production??

  last_share_import <- collapseNames(pulpwood[, 1989, "import_m3"] / pulpwood[, 1989, "production"])
  last_share_import[is.na(last_share_import)] <- 0
  last_share_import[last_share_import > 1] <- 1

  pulpwood[, 1990:2019, "export_m3"] <- pulpwood[, 1990:2019, "production"] * last_share_export
  pulpwood[, 1990:2019, "import_m3"] <- pulpwood[, 1990:2019, "production"] * last_share_import
  ## Pulpwood export import data is not reported

  ## remove pulpwood data fom raw data momentarily
  x <- x[, , pulpwood_names, invert = TRUE]

  ## Add back pulpwood
  x <- mbind(x, pulpwood)

  # Sort naming
  x <- x[, , sort(getNames(x, dim = 1))]

  ## Extract variables we need and only choose variables which which have data for all three categories:
  ## Production in m3
  ## Import in m3
  ## Export in m3
  ## Here, wood pulp is an exception which is in mio. tonnes so we will assume 450 kg/m3 density there

  variables_needed <- c(
    "Roundwood",
    "Industrial roundwood", "Wood fuel",
    "Other industrial roundwood", "Pulpwood",
    "Sawlogs and veneer logs", "Fibreboard", "Particle board and OSB",
    "Wood pulp", "Sawnwood", "Plywood", "Veneer sheets", "Wood-based panels"
  )

  if (length(variables_needed[!(variables_needed %in% getNames(x, dim = 1))]) > 0) message("Category '", variables_needed[!(variables_needed %in% getNames(x, dim = 1))], "' does not exist in source FAO data. Manual correction to code will be needed.")
  ## in above variables, remeber that Sawlogs and veneer logs need an additional category called "SLVL based wood" which we will calculate back based on the sum of Sawnwood, plywood, venner sheets, wood residues and wood chips.
  ## Also remeber that Pulpwood round and split only has data for production and not import and export so that has to be calculated back based on its constituents.
  ## See https://i.imgur.com/VackFiv.png for better clarity.

  timber_fao <- x[, , variables_needed] ### Remember that wood pulp is in mio tonnes so we have to convert it to m3

  ## We'll consider the production, import and export separately

  production <- timber_fao[, , grep(pattern = "production", x = getNames(timber_fao, dim = "ElementShort"), value = TRUE)]
  import <- timber_fao[, , grep(pattern = "import", x = getNames(timber_fao, dim = "ElementShort"), value = TRUE)]
  export <- timber_fao[, , grep(pattern = "export", x = getNames(timber_fao, dim = "ElementShort"), value = TRUE)]

  ## Next step is to convert wood pulp from t to m3

  production[, , "Wood pulp"] <- production[, , "Wood pulp"] * 1000 / 450 ## 10^3 for t to kg. 450 for kg to m3
  import[, , "Wood pulp"] <- import[, , "Wood pulp"] * 1000 / 450 ## 10^3 for t to kg. 450 for kg to m3
  export[, , "Wood pulp"] <- export[, , "Wood pulp"] * 1000 / 450 ## 10^3 for t to kg. 450 for kg to m3

  ## Remove all units as now everything is converted to m3
  getNames(production, dim = "ElementShort") <- gsub(pattern = "_m3", replacement = "", x = getNames(production, dim = "ElementShort"))
  getNames(import, dim = "ElementShort") <- gsub(pattern = "_m3", replacement = "", x = getNames(import, dim = "ElementShort"))
  getNames(export, dim = "ElementShort") <- gsub(pattern = "_m3", replacement = "", x = getNames(export, dim = "ElementShort"))
  ## in the import and export objects, we need to add segment for pulpwood which is the sum of fibreboard, particle board OSB and Wood pulp

  to_add <- setdiff(getNames(production, dim = 1), getNames(export, dim = 1))

  import <- add_columns(x = import, addnm = to_add, dim = 3.1)
  export <- add_columns(x = export, addnm = to_add, dim = 3.1)

  import[, , to_add] <- 0 ## No import and export data for pulpwood from FAO
  export[, , to_add] <- 0 ## No import and export data for pulpwood from FAO

  ## Another category to add here is "Other Sawnwood" (refer to ??)
  to_add <- "Other sawnwood"
  production <- add_columns(x = production, addnm = to_add, dim = 3.1)
  production[, , to_add] <- production[, , "Sawlogs and veneer logs"] - dimSums(production[, , c("Plywood", "Veneer sheets", "Sawnwood")], dim = 3)
  if (min(production[, , to_add]) < 0) message("Negative values detected when adding category '", to_add, "'. Setting negative values to 0.")
  production[, , to_add][production[, , to_add] < 0] <- 0
  import <- add_columns(x = import, addnm = to_add, dim = 3.1)
  export <- add_columns(x = export, addnm = to_add, dim = 3.1)
  import[, , to_add] <- 0 ## No import and export data for Other sawnwood from FAO
  export[, , to_add] <- 0 ## No import and export data for Other sawnwood from FAO

  ###### Correction Stage
  ###### Refer to http://dx.doi.org/10.1016/j.rser.2016.09.107
  ###### Paper titled "The wood from the trees: The use of timber in construction"
  indicator_list <- c()
  for (indicator in getNames(production, dim = "ItemCodeItem")) {
    incorrect_data <- where(production[, , indicator][, , "production"] < export[, , indicator][, , "export"])$true
    if (length(unique(incorrect_data$regions)) > 0) indicator_list <- c(indicator_list, indicator)
    export[, , indicator][, , "export"] <- production[, , indicator][, , "production"]
  }
  message("Higher exports than production level detected in some countries. Setting these export values to production value. Following categories affected: ", paste0(indicator_list, ", "))

  ## Now we are ready to merge back production, import and export
  timber_fao_cleaned <- mbind(production, import, export)

  ###### Data cleaning stage
  timber_fao_cleaned <- add_columns(timber_fao_cleaned, addnm = "other_util", dim = 3.2)
  timber_fao_cleaned[, , "other_util"] <- timber_fao_cleaned[, , "production"] + timber_fao_cleaned[, , "import"] - timber_fao_cleaned[, , "export"]

  timber_fao_cleaned <- add_columns(timber_fao_cleaned, addnm = "domestic_supply", dim = 3.2)
  timber_fao_cleaned[, , "domestic_supply"] <- timber_fao_cleaned[, , "other_util"]

  ## Now convert data to mio. m3
  timber_fao_cleaned <- timber_fao_cleaned / 1e6

  ## Check if negatvive values still exist
  if (min(timber_fao_cleaned) < 0) {
    message("CAUTION: Negative values still exist in cleaned FAO timber production-import-export data.")
    timber_fao_cleaned[timber_fao_cleaned < 0] <- 0
    message("Such values are set to 0 now but could be problematic. Check R Script which processes this data")
  }

  out <- timber_fao_cleaned

  return(list(
    x = out,
    weight = NULL,
    min = 0,
    unit = "mio m3",
    description = "Calculates the timber demand based on historical FAO data"
  ))
}
