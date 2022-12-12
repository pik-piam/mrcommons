#' @title readEDGAR6
#' @description download EDGAR 5 and 6 emission data
#' @param subtype type in gas
#' @return List of magpie objects with results on country level, weight, unit and description.
#' @author Benjamin Leon Bodirsky, Michael S. Crawford
#' @importFrom readxl read_excel

readEDGAR6 <- function(subtype) {

  files <- c(n2o              = "v60_N2O_1970_2018.xls",
             ch4              = "v60_CH4_1970_2018.xls",
             co2_excl_short   = "v60_CO2_excl_short-cycle_org_C_1970_2018.xls",
             co2_incl_short   = "v60_CO2_org_short-cycle_C_1970_2018.xls",
             nh3              = "v50_NH3_1970_2015.xls",
             no2              = "v50_NOx_1970_2015.xls",
             bc               = "v50_BC_1970_2015.xls",
             co               = "v50_CO_1970_2015.xls",
             oc               = "v50_OC_1970_2015.xls",
             nmvoc            = "v50_NMVOC_1970_2015.xls",
             pm10             = "v50_PM10_1970_2015.xls",
             pm25             = "v50_PM2.5_1970_2015.xls",
             so2              = "v50_SO2_1970_2015.xls")

  skip <- c(n2o            = 8,
            ch4            = 8,
            co2_excl_short = 8,
            co2_incl_short = 8,
            nh3            = 8,
            no2            = 8,
            bc             = 8,
            co             = 8,
            oc             = 8,
            nmvoc          = 8,
            pm10           = 8,
            pm25           = 8,
            so2            = 8)

  file <- toolSubtypeSelect(subtype, files)

  # add read function that copes with different data input types
  ed <- as.data.frame(read_excel(file, skip = skip[subtype], na = "NULL"), stringsAsFactors = FALSE)

  ed[, 6] <- paste(ed[, 5], ed[, 6], sep = " ")
  ed[, 6] <- gsub(pattern = "\\.", replacement = "_", x = ed[, 6])
  ed <- ed[, -c(1, 2, 4, 5)]
  colnames(ed) <- gsub(pattern = "Y_", replacement = "y", x = colnames(ed))

  out <- as.magpie(ed, spatial = 1)

  # EDGAR6 contains two other sources, "AIR" (Int. aviation) and "SEA" (Int. shipping)
  out <- out[!(getItems(out, dim = 1) %in% c("AIR", "SEA")), , ]

  out <- add_dimension(out, dim = 3.1, add = "pollutant", nm = subtype)

  return(out)

}
