#' toolAdjustExperGuesses
#' @description Function that uses ssp2 as baseline scenario and adjusts ssp1,ssp3,ssp4, and ssp5 based on
#' pre-established assumptions about the relative development in the different scenarios.
#' @param q magpie object with expert guesses loaded
#'
#' @return Adjusted expert guesses
#' @author Marcos Alves
#'
#' @export

toolAdjustExperGuesses <- function(q) {
  # Reading expert guesses about livesotck productivity for each SSP and storing all SSPs in a magpie Object
  #  q <- readSource("ExpertGuessSSPLivestockProduction", subtype = paste0("2023", ":", "ssp1"))
  #  q <- add_dimension(q, dim = 3.2, add = "scen", nm = "ssp1")
  #  for (i in 2:5) {
  #    ssp <- paste0("ssp", i)
  #    q <- add_columns(q, dim = 3.2, addnm = ssp)
  #    q[, , ssp] <- readSource("ExpertGuessSSPLivestockProduction", paste0("2023", ":", ssp))
  #  }

  # Recovering the labels that are used as proxis for the development stage
  labels <- list()
  for (i in getItems(q, dim = 3.1)) {
    labels[[i]] <- sort(unique(as.vector(q[, , i])))
  }

  x <- q
  x[, , ] <- NA

  # Transforming the numbers used to tag the different pathways to indexes.

  for (j in getItems(q, dim = 3.1)) {
    levels <- sort(unique(as.factor(q[, , j])))
    tmp <- q[, , j]
    for (i in 1:length(levels)) {
      tmp1 <- tmp == levels[i]
      tmp[tmp1] <- i
      x[, , j] <- tmp
    }
  }

  # Recovering the indexes after conversion of q in x
  indexes <- list()
  for (i in getItems(x, dim = 3.1)) {
    indexes[[i]] <- 1:max(unique(as.vector(x[, , i])))
  }

  # Making adjustments on SSP3 based on the reference values of SSP2:
  #  -- Assumption: All regions in all times performe worse in ssp3 than in ssp2
  i <- 1
  while (i > 0) {
    w <- collapseNames(x[, , "ssp2"] <= x[, , "ssp3"])       # checking which ssp3 values are bigger or equal to its correspondent ssp2 values.
    z <- collapseNames(x[, , "ssp3"] > 1)                    # checking which values in ssp3 are already not in the minimum level.
    t <- w * z
    xtmp <- x[, , "ssp3"]
    xtmp[!!t] <- xtmp[!!t] - 1                               # Reducing one level on ssp3 in the regions where ssp3 values are bigger or equal to ssp3 and are not minimal already
    x[, , "ssp3"] <- xtmp
    i <- sum(t)                                              # count how many countries still need to be adjusted, run until 0
  }

  # Making adjustments on SSP5 based on the reference values of SSP2:
  #  -- Assumption: All regions in all times performe better in ssp5 than in ssp2
  i <- 1
  while (i > 0) {
    w <- collapseNames(x[, , "ssp5"] <= x[, , "ssp2"])       # checking which ssp2 values are bigger or equal to its correspondent ssp5 values.
    z <- w
    z[, , ] <- NA
    for (j in getItems(x, dim = 3.1)) {                      # checking which values in ssp5 are already at the maximum level for each sys.
      z[, , j] <- collapseNames(x[, , list(data = j, scen = "ssp5")] < max(indexes[[j]]))
    }
    t <- w * z
    xtmp <- x[, , "ssp5"]
    xtmp[!!t] <- xtmp[!!t] + 1                               # Increasing one level in ssp5 regions where ssp2 values are bigger or equal to ssp5 and are not maximal already
    x[, , "ssp5"] <- xtmp
    print(i)
    i <- sum(t)                                              # count how many countries still need to be adjusted, run until 0
  }

  # determining development level of countries
  gdp_pc <- calcOutput("GDPpc", naming = "scenario", aggregate = FALSE, supplementary = TRUE)
  gdp_pc <- collapseNames(gdp_pc$x)
  developed  <- middle  <- lower <- gdp_pc[, getItems(x, dim = 2), toupper(getItems(x, dim = 3.2))]
  for (i in getItems(x, dim = 2)) {
    for (j in toupper(getItems(x, dim = 3.2))) {
      developed[, i, j] <- gdp_pc[, i, j] >= quantile(gdp_pc[, i, j])[4]
      middle[, i, j] <- !!(gdp_pc[, i, j] < quantile(gdp_pc[, i, j])[4]) * (gdp_pc[, i, j] >= quantile(gdp_pc[, i, j])[3])
      lower[, i, j] <- gdp_pc[, i, j] < quantile(gdp_pc[, i, j])[3]
    }
  }
  getItems(developed, dim = 3) <- getItems(x, dim = 3.2)
  getItems(middle, dim = 3) <- getItems(x, dim = 3.2)
  getItems(lower, dim = 3) <- getItems(x, dim = 3.2)

  # Making adjustments on SSP1:
  #  -- Assumption: Same to ssp2 for more Developed and same ssp5 for Developing (middle and lower range).
  x[, , "ssp1"] <- x[, , "ssp2"] * developed[, , "ssp1"] + x[, , "ssp5"] * middle[, , "ssp1"] + x[, , "ssp5"] * lower[, , "ssp1"]

  # Making adjustments on SSP4:
  #  -- Assumption:  Same to ssp5 for more Developed, same ssp2 for middle and ssp3 for lower development.
  x[, , "ssp4"] <- x[, , "ssp5"] * developed[, , "ssp4"] + x[, , "ssp2"] * middle[, , "ssp4"] + x[, , "ssp3"] * lower[, , "ssp4"]

  for (i in getItems(x, dim = 3.1)) {
    x[, , i] <- labels[[i]][x[, , i]]
  }

  return(x)
}
