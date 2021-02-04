#' @title readHerridge
#' @description Reads a dataset containing values for biological nitrogen fixation in agricultural systems. Source: Herridge D. F., Peoples M. B., Boddey R. M.: Global inputs of biological nitrogen fixation in agricultural systems
#' @details Availables Subtypes:
#'  \itemize{
#'  \item ndfa:  National values for Plant associated fixation
#'  \item freeliving: Global values for free living agents
#'  }
#' @param subtype a subtype for the calculation
#' @return A MAgPIE object containting the share of Nr derived from
#' \itemize{
#' \item ndfa: fixation for each country and each commodity.
#' \item freeliving: fixation by free living agents
#' }
#' @author Stephen Wirth, Jan Philipp Dietrich
#' @examples
#' 
#'   \dontrun{
#'     x <- readSource("Herridge", "ndfa")
#'     x <-  readSource("Herridge", "freeliving", convert=F)
#'   }
#' 
readHerridge <- function(subtype=NULL){

  #file to read
  files <- c(ndfa="ndfa.csv",
             freeliving ="freeliving.csv")
  
  file <- toolSubtypeSelect(subtype,files)
  #read file
  if (subtype=="ndfa"){
    data <- read.csv(file = file, header = T,stringsAsFactors = F, skip=4)
  } else if (subtype=="freeliving") {
    data <- read.csv(file=file, header=F, stringsAsFactors = F)
  } else {
    stop("Unsupported type!")
  }
  d <- as.magpie(data)
  getYears(d) <- "y2005"
  getSets(d)  <- c("region", "year", ifelse(subtype=="ndfa","groups","data"))
  return(d)
   
}