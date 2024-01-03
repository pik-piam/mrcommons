#' @title calcAnimalStocks
#' @description calculates stocks of animals of different categories.
#'
#' @param grouping IPCC: Animal grouping of IPCC Guidelines
#'
#' @return List of magpie objects with results on country level, weight on country level, unit and description.
#' @author Benjamin Leon Bodirsky
#' @seealso
#' [calcExcretionIPCC()],
#' [readIPCC()]
#' @examples
#' \dontrun{
#' calcOutput("AnimalStocks")
#' }
#'
calcAnimalStocks <- function(grouping = "IPCC") {
  if (grouping != "IPCC") {
    stop("so far only IPCC categories implemented.")
  }

  marketSwineShare <- 0.9 # table 10.19

  liveHead <- dimSums(readSource("FAO_online", "LiveHead"), dim = "ElementShort")
  livePrim <- readSource("FAO_online", "LivePrim")
  years <- intersect(getYears(liveHead), getYears(livePrim))
  liveHead <- liveHead[, years, ]
  livePrim <- livePrim[, years, ]

  # estimate numbers of animals for IPCC categories
  animals <- NULL

  # Dairy cows
  animals <- mbind(animals, setNames(collapseNames(livePrim[, , c("Milk_Animals_(Head)")]
  [, , c("882|Milk, whole fresh cow")]), "dairy cows"))
  # Other cattle
  animals <- mbind(animals, setNames(
    collapseNames(liveHead[, , c("866|Cattle")])
    - setNames(animals[, , "dairy cows"], NULL),
    "other cattle"
  ))

  # Dairy Buffalo
  animals <- mbind(animals, setNames(collapseNames(livePrim[, , c("Milk_Animals_(Head)")]
                                                   [, , c("951|Milk, whole fresh buffalo")]), "dairy buffalo"))
  # Other buffalo
  animals <- mbind(animals, setNames(collapseNames(liveHead[, , c("946|Buffaloes")]) -
                                       setNames(animals[, , "dairy buffalo"], NULL), "other buffalo"))

  # Market Swine
  animals <- mbind(animals, setNames(
    collapseNames(liveHead[, , c("1034|Pigs")])
    * marketSwineShare,
    "market swine"
  ))

  # Breeding Swine
  animals <- mbind(animals, setNames(
    collapseNames(liveHead[, , c("1034|Pigs")])
    * (1 - marketSwineShare),
    "breeding swine"
  ))

  # Dairy Sheep
  animals <- mbind(animals, setNames(collapseNames(livePrim[, , c("Milk_Animals_(Head)")]
  [, , c("982|Milk, whole fresh sheep")]), "dairy sheep"))
  # Other sheep
  animals <- mbind(animals, setNames(
    collapseNames(liveHead[, , c("976|Sheep")])
    - setNames(animals[, , "dairy sheep"], NULL),
    "other sheep"
  ))

  # Dairy Goats
  animals <- mbind(animals, setNames(collapseNames(livePrim[, , c("Milk_Animals_(Head)")]
  [, , c("1020|Milk, whole fresh goat")]), "dairy goats"))
  # Other goats
  animals <- mbind(animals, setNames(
    collapseNames(liveHead[, , c("1016|Goats")])
    - setNames(animals[, , "dairy goats"], NULL),
    "other goats"
  ))

  # Camels
  animals <- mbind(animals, setNames(collapseNames(livePrim[, , c("Milk_Animals_(Head)")]
  [, , c("1130|Milk, whole fresh camel")]), "dairy camels"))
  # Other Camelids
  animals <- mbind(animals, setNames(
    dimSums(liveHead[, , c("1126|Camels", "1157|Camelids, other")], dim = 3.1)
    - setNames(animals[, , "dairy camels"], NULL),
    "other camels"
  ))

  # Horses
  animals <- mbind(animals, setNames(
    collapseNames(liveHead[, , c("1096|Horses")]),
    "horses"
  ))

  # Mules and Asses
  animals <- mbind(animals, setNames(
    dimSums(liveHead[, , c("1107|Asses", "1110|Mules")], dim = 3.1),
    "mules and asses"
  ))

  # Poultry Layers
  animals <- mbind(animals, setNames(collapseNames(dimSums(livePrim[, , c("Laying_(Head)")]
  [, , c("1062|Eggs, hen, in shell", "1091|Eggs, other bird, in shell")], dim = 3.1)), "poultry layers"))
  # Broilers
  animals <- mbind(animals, setNames(
    dimSums(liveHead[, , c("1057|Chickens", "1083|Pigeons, other birds")], dim = 3.1)
    - setNames(animals[, , "poultry layers"], NULL),
    "broilers"
  ))

  # Turkey
  animals <- mbind(animals, setNames(
    dimSums(liveHead[, , c("1072|Geese and guinea fowls", "1079|Turkeys")], dim = 3.1),
    "turkey"
  ))

  # Ducks
  animals <- mbind(animals, setNames(
    collapseNames(liveHead[, , c("1068|Ducks")]),
    "ducks"
  ))

  # ignore
  # "1140|Rabbits and hares","1150|Rodents, other"
  animals <- animals / 1000000

  # sort according to n_rate animal categories


  # remove all negative values
  remove <- which(animals < 0)

  if (length(remove) > 0) {
    vcat(2, paste0(length(remove), " negative values removed"))
    animals[remove] <- 0
  }

  return(list(
    x = animals,
    weight = NULL,
    unit = "Million animals",
    description = "Animal stocks, for laying hens and dairy cattle producing animals",
    min = 0
  ))
}
