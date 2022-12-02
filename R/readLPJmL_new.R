#' @title readLPJmL_new
#'
#' @description Read in LPJmL outputs
#'
#' @param subtype Switch between different inputs
#'                (eg. "LPJmL5.2_Pasture:IPSL_CM6A_LR:ssp126_co2_limN_00:soilc_past_hist")
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#'
#' @author Kristine Karstens, Abhijeet Mishra, Felicitas Beier, Marcos Alves
#'
#' @seealso
#' [readLPJ()]
#' @examples
#' \dontrun{
#' readSource("LPJmL_new", convert = FALSE)
#' }
#'
#' @importFrom madrat toolSplitSubtype
#' @importFrom magpiesets addLocation
#' @importFrom lpjclass readLPJ
#' @importFrom stringr str_subset str_trim str_split

readLPJmL_new <- function(subtype = "LPJmL4_for_MAgPIE_44ac93de:GSWP3-W5E5:historical:soilc") {
  subtype <- toolSplitSubtype(
    subtype,
    list(version = NULL, climatemodel = NULL, scenario = NULL, variable = NULL)
  )$variable

  .prepareLPJ <- function(datatype = numeric(),
                          bytes = 4,
                          monthly = FALSE,
                          nbands = NULL) { # nbands will be overwritten for clm data

    file_name <- Sys.glob(c("*.bin", "*.clm"))
    file_type <- tail(unlist(strsplit(file_name, "\\.")), 1)

    if (file_type == "clm") {
      filedata <- file(description = file_name,
                       open = "rb",
                       blocking = TRUE,
                       encoding = getOption("encoding"))
      seek(filedata, where = 15, origin = "start")
      in_header <- as.numeric(readBin(filedata,
                                      what = integer(),
                                      size = 4,
                                      n = 5,
                                      endian = .Platform$endian))
      startYear <- in_header[1]
      nyear <- in_header[2]
      nbands <- in_header[5] # nbands will be overwritten for clm data
      years <- seq(startYear, startYear + nyear - 1, 1)
      headlines <- 51 # generation clm 3
      close(filedata)
    } else if (file_type == "bin") {
      outfile <- grep(".out", list.files(), value = TRUE) %>% head(1)
      out <- readLines(outfile)
      startYear <- out %>%
        str_subset("Output written in year:") %>%
        str_split(":") %>%
        unlist() %>%
        str_trim() %>%
        subset(c(FALSE, TRUE)) %>%
        as.numeric()
      endYear <- out %>%
        str_subset("Last year:") %>%
        str_split(":") %>%
        unlist() %>%
        str_trim() %>%
        subset(c(FALSE, TRUE)) %>%
        as.numeric()
      years <- seq(startYear, endYear, 1)
      headlines <- 0
    } else {
      stop("File format of LPJmL input data unknown. Please provide .clm or .bin file format.")
    }

    x <- readLPJ(
      file_name       = file_name,
      wyears          = years,
      syear           = startYear,
      headlines       = headlines,
      averaging_range = 1,
      ncells          = 67420,
      file_type       = "bin",
      bands           = nbands,
      datatype        = datatype,
      bytes           = bytes,
      monthly         = monthly
    )

    class(x) <- "array"
    x <- collapseNames(as.magpie(x, spatial = 1))
    x <- collapseDim(addLocation(x), dim = "N")
    x <- clean_magpie(x)

    return(x)
  }

  if (subtype %in% c(
    "soilc", "litc", "vegc", "alitfallc", "aet",
    "vegc_grass", "litc_grass", "soilc_grass", "aprec", "soilc_past_hist", "soilc_past_scen"
  ) |
    grepl("alitter", subtype)) {
    x <- .prepareLPJ(nbands = 1)
  } else if (grepl("*date*", subtype)) {
    x <- .prepareLPJ(nbands = 24, datatype = integer(), bytes = 2)
  } else if (subtype %in% c("soilc_layer", "cshift_slow", "cshift_fast")) {
    x <- .prepareLPJ(nbands = 5)
  } else if (grepl("mdischarge|mrunoff|mpet|mgpp_grass_ir|mgpp_grass_rf|met_grass_ir|met_grass_rf", subtype)) {
    x <- .prepareLPJ(monthly = TRUE)
  } else if (grepl("harvest|irrig|cwater_b|grass_pft|cft_gpp_grass_rf|cft_gpp_grass_ir|cft_et_grass_rf|cft_et_grass_ir", subtype)) {
    x <- .prepareLPJ(nbands = 32)
  } else if (grepl("fpc", subtype)) {
    x <- .prepareLPJ(nbands = 12)
  } else {
    stop(paste0("subtype ", subtype, " is not existing"))
  }

  return(round(x, digits = 10))
}
