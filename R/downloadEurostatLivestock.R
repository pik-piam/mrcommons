#' Download Eurostat Livestock Data
#'
#' Downloads the latest data and meta data form the Eurostat website.
#'
#' @param subtype Type of Eurostat Livestock data that should be read.
#'
#' @importFrom utils download.file

downloadEurostatLivestock <- function(subtype) {
  # nolint: object_name_linter.
  files <- c(
    EggPopProd = "apro_ec_egghen",
    # number of prod eggs, number of laying hens, both quite incomplete
    LayingHensPop = "apro_ec_lshen",
    # very incomplete, especially for later years
    PoultryPop = "apro_ec_poula",
    # some missing entries, includes broilers and laying hens
    PigPop = "apro_mt_lspig",
    # unit 1000 heads, almost complete
    BovinePop = "apro_mt_lscatl",
    # unit 1000 heads, almost complete, incl dairy cows
    Nuts2Pop = "agr_r_animal",
    # comprehensive, but a lot of missing entries, DE almost compl missing
    MeatProd = "apro_mt_pheadm",
    # very comprehensive and complete, heads and tons, poultry chick, bovine,
    # pig
    MilkNuts2Prod = "agr_r_milkpr",
    # unit 1000 tons, some missing entries but still of worth
    MilkProd = "apro_mk_farm"
    # unit 1000, almost complete, raw milk prod and delivered
  )

  # select file
  file <- toolSubtypeSelect(subtype, files)

  # --- get data
  # compose url
  urlBase <-
    "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/"
  urlPartData <- "data/"
  urlFormat <- "?format=TSV&compressed=false"
  url <- paste0(urlBase, urlPartData, file, urlFormat)

  # download file
  download.file(url, destfile = paste0(file, ".tsv"))

  # --- get metadata
  # compose url
  urlPartMetaFlow <-
    "dataflow/ESTAT/"
  urlMetaFlow <- paste0(urlBase, urlPartMetaFlow, file)

  # get meta xml
  metaFlow <-  paste0(file, "_dataflow.xml")
  download.file(urlMetaFlow, destfile = metaFlow)
  metaFlowList <- XML::xmlToList(metaFlow)

  # extract all the necessary meta data
  metaFlowDataflow <- metaFlowList$Structures$Dataflows$Dataflow
  version <- metaFlowDataflow$Structure$Ref["version"]
  longName <- metaFlowDataflow[3]$Name$text
  dateUpdate <- metaFlowDataflow$Annotations[5]$Annotation$AnnotationTitle

  # delete the xml
  unlink(metaFlow)


  # --- return data
  return(
    list(
      url           = url,
      doi           = "not available",
      title         = file,
      author        = "not avaiblable",
      version       = version,
      release_date  = dateUpdate,
      description   = longName,
      license       = "https://ec.europa.eu/eurostat/about-us/policies/copyright",
      reference     = "not available",
      unit          = "not available"
    )
  )

}
