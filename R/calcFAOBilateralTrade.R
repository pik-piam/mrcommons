#' @title calcFAOBilateralTrade
#' @description Calculates bilateral trade values based on FAO trade matrix
#' @param output "value", "qty", or "price" 
#' @param products "kcr", "kli", or "kothers"
#' @param prod_agg binary to keep FAO product level or magpie 
#' @param five_year only 5 year steps due to memory load
#' @return List of magpie objects with results on bilateral country level, weight on bilateral country level, unit and description.
#' @author David M Chen
#' @examples
#'
#' \dontrun{
#' calcOutput("BilateralTradeValue")
#' }
#'
calcFAOBilateralTrade <- function(output = "value", products = "kcr", prod_agg = TRUE, five_year = TRUE) {

#### harmonize export and import-based reporting based on reliability index (Gelhar 1996)
#importer and exporter datasets

if (output %in% c("qty", "value")){

im <- collapseNames(readSource("FAO_TradeMatrix", subtype = paste("import", output, products, sep = "_"), convert = TRUE))
im <- im[,c(min(getYears(im, as.integer = TRUE)):1994), inv = T] #subset years for lighter load on mem
ex <- collapseNames(readSource("FAO_TradeMatrix", subtype = paste("export", output, products, sep = "_"), convert = TRUE))
ex <- ex[,c(min(getYears(ex, as.integer = TRUE)):1994), inv = T]
  
  if(five_year){
   im <- im[,seq(1995,2020,5),]
   ex <- ex[,seq(1995,2020,5),]
  }

 .harmBilat <- function(im, ex, value){
  
  if(value){
  #imports generally reported on cif basis, use generic 12% (FAOSTAT) for now.
  fobCvn <- 1.12
  #convert exporter values to cif
  ex <- ex * fobCvn
  }
#remove missing items from intersect for now
 citems <- intersect(getNames(im), getNames(ex))
 im <- im[,,citems]
 ex <- ex[,,citems]

 ####determine reliability index of countries imports and exports reporting (Gelhar 1996)
  # re-order exports so it's reporter country second, so exports imports in same dim order
  ex <- dimOrder(ex, perm = c(2,1), dim = 1)
  #create accuracy level of each commodity-countrypair
  accLevel <- abs((im - ex)/im)
  #total
  gc()
  imTot <- dimSums(im, dim = 1.2)
  gc()
  ## use reliability as Accuracy Level accLevel within =< 0.20 (Gelhar 1996) 
  # get all trades within 0.2 accuracy (0 most accurate)
  imAcc <- im
  imAcc[which(accLevel > 0.2)] <- 0
  gc()
  imAcc <- dimSums(imAcc, dim = 1.2)
  #reliability index
  RIM <- imAcc/imTot * 100
  gc()

  exTot <- dimSums(ex, dim = 1.1)
  gc()

  exAcc <- ex
  exAcc[which(accLevel > 0.2)] <- 0
  gc()
  exAcc <- dimSums(exAcc, dim = 1.1)
  gc()
  #reliability index
  RIX <- exAcc/exTot * 100
  gc()
  # make difference in reliability for all country combinations
  getItems(RIX, dim = 1) <- paste0(getItems(RIX, dim=1), "1")
  imR <- exR <- RIM - RIX
  getItems(imR, dim = 1) <- gsub("1", "", getItems(imR, dim = 1)) 
  getItems(imR, dim = 1, raw = TRUE) <- gsub("p", "\\.", getItems(imR, dim = 1)) 
  getItems(exR, dim = 1) <- gsub("1", "", getItems(exR, dim = 1)) 
  getItems(exR, dim = 1, raw = TRUE) <- gsub("p", "\\.", getItems(exR, dim = 1)) 
  gc()
  exR[which(exR >= 0)] <- 0
  exR[which(exR < 0)] <- 1
  imR[which(imR >= 0)] <- 1
  imR[which(imR < 0)] <- 0
 gc()

  imR_only <- im*imR 
  exR_only <- ex*exR
  gc()

  out <- imR_only + exR_only
  return(out)
}

if(output == "qty") {
  value <- FALSE
} else if (output == "value"){
  value <- TRUE
}
 out <- .harmBilat(ex = ex, im = im, value = value)
 weight <- NULL

 if (output == "qty"){
  out <- out/1e6 # convert million tonnes
  unit <- "MtWM"
  } else if (output == "value"){
   out <- out/1e3 # in millions
   unit <- "million USD$05"
  }

} else if (output == "price"){
qty <- calcOutput("FAOBilateralTrade", output = "qty", products = products, prod_agg = FALSE, aggregate = FALSE)
value <- calcOutput("FAOBilateralTrade", output = "value", products = products, prod_agg = FALSE, aggregate = FALSE)
out <- value/qty
weight <- qty
unit <- "US$05/tDM"
}

 if(prod_agg){
 # aggregate to get a preliminary cif/fob ratio
 out[is.na(out)] <- 0
 mapping <- toolGetMapping("newFAOitems_online_DRAFT.csv", type = "sectoral")
 out <- toolAggregate(out, rel = mapping, from = "new_FAOoriginalItem_fromWebsite",
                           to = "k" , partrel = T, dim = 3.1)
    
    if (output == "qty") {
    attr <- calcOutput("Attributes", aggregate = FALSE)
    out <- out / collapseNames(attr[,,"wm"][,,getNames(out)])
    unit <- "MtDM"
    } else if (output == "price") {
    attr <- calcOutput("Attributes", aggregate = FALSE)
    out <- out * collapseNames(attr[,,"wm"][,,getNames(out)])
    unit <- "tDM"
    }
 }

 getSets(out)[c(1,2)] <- c("im", "ex")
 out[is.na(out)] <- 0
 out[is.infinite(out)] <- 0
 
  return(list(x = out,
              weight = weight,
              unit = unit,
              description = "Bilateral Trade values")
  )
}
