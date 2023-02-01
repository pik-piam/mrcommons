#' @title downloadGGCMICropCalendar
#' @description Download ISIMIP GGCMI crop calendar information and harvest area masks
#'
#'
#' @author David M Chen, Edna Molina Bacca

downloadGGCMICropCalendar <- function() {
  # spring and winter wheat
  wheatfile <- "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/landuse/winter_spring_wheat_separation/winter_and_spring_wheat_areas_phase3.nc4"

  if (file.exists(wheatfile)) {
    file.copy(wheatfile, basename(wheatfile))
  } else {
    vcat(1, paste0("Data for wheat masks could not be found!"))
  }

  for (crop in c("bar", "bea", "cas", "cot", "mai", "mil",
                 "nut", "pea", "pot", "rap", "ri1", "ri2", "rye",
                 "sgb", "sgc", "sor", "soy", "sun", "swh", "wwh")) {
    for (irr in c("ir", "rf")) {

      path <- "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/crop_calendar"

      file <-  file.path(path, paste0(crop, "_", irr, "_ggcmi_crop_calendar_phase3_v1.01.nc4"))

      if (file.exists(file)) {
        file.copy(file, basename(file))
      } else {
        vcat(1, paste0("Data for requested subtype \"", path, "\" could not be found!"))
      }
    }
  }

  return(list(
    url          = "/p/projects/macmit/data/GGCMI/AgMIP.input/phase3/crop_calendar",
    title        = "Crop calendar information for GGCMI",
    description  = "harvested area fraction, Planting day, maturity day, growing seasons length, and data source",
    revision     = "Phase3_v1.01",
    comment      = NULL,
    unit         = "day of year",
    release_date = "2020",
    license      = NULL,
    author       = "GGCMI"))
}
