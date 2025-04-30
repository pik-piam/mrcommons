#' @title Read GFED
#' @description Read the Global Fire Emissions Database over the available years and store
#' them in a MAgPIE object with two sub-dimensions: Species (e.g. DM, CO2, CH4, or N2O),
#' and Partition (e.g. SAVA, BORF, TEMF, DEFO, PEAT, AGRI). Emissions are reported in Mt X.
#' For more information, see: https://globalfiredata.org/pages/data/. Due to runtime considerations,
#' this function only calculates a small sample of the total emissions (CH4, N2O, NOx, NH3) and
#' burning partitions (AGRI), which are currently being used for validation.
#'
#' @return A MAgPIE object with the GFED emissions data and sub-dimensions Partition and Species.
#' @author Michael S. Crawford
#'
#' @seealso [madrat::readSource()]
#' @examples \dontrun{
#' a <- readSource("GFED")
#' }
#'
#' @importFrom hdf5r h5file
#' @importFrom stringr str_extract
#' @importFrom dplyr %>% mutate select filter rename right_join inner_join
#' @importFrom purrr walk map map2 pmap
#' @importFrom tidyr expand_grid pivot_longer unnest
#' @importFrom reshape2 melt
#' @importFrom madrat toolGetMapping
#' @importFrom magclass as.magpie
#' @importFrom rlang .data

readGFED <- function() {
  # Functions ---------------------------------------------------------------

  # Sums the total DM emissions across a given year for each source partition and
  # calculates Mt DM per 0.25x0.25 degree cell from kg DM m^-2 per 0.25x0.25 degree cell.
  .calculateYear <- function(year, emission, partition) { # nolint: object_usage_linter.
    h5dData <- h5file(filename = paste0("GFED4.1s_", year, ".hdf5"))

    # Sum the emissions across months for this emission|partition
    monthIds <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
    yearTotal <- matrix(nrow = 1440, ncol = 720, data = 0)
    walk(.x = monthIds,
         .f = ~ {
           # Get total DM emissions
           dmEmissionsId <- paste0("emissions/", .x, "/", emission)
           dmEmissions <- h5dData[[dmEmissionsId]]$read()

           # Get the partition's contributions
           partitionId <- paste0("emissions/", .x, "/partitioning/", emission, "_", partition)
           partitionFraction <- h5dData[[partitionId]]$read()

           yearTotal <<- yearTotal + (dmEmissions * partitionFraction) # nolint: undesirable_operator_linter.
         })

    # kg DM m^-2 to kg DM
    sqMetersPerGridcell <- h5dData[["ancill/grid_cell_area"]]$read()
    yearTotal <- yearTotal * sqMetersPerGridcell

    # kg DM to Mt DM
    yearTotal <- yearTotal * 1E-09

    # The GFED datasets mistakenly have 720 cols and 1440 rows, rather than 1440 col and 720 rows.
    yearTotal <- t(yearTotal)

    return(yearTotal)
  }

  # GFED data is reported in 0.25x0.25 degree grid cells. This function rescales the
  # data to fit with MAgPIE, by summing four grid cells into one 0.5x0.5 degree grid cell,
  # and returns the data in longitude-latitude format.
  .rescaleGrid <- function(mat) { # nolint: object_usage_linter.
    mat1 <- mat[seq(1, dim(mat)[1], by = 2), seq(1, dim(mat)[2], by = 2)] # top left
    mat2 <- mat[seq(2, dim(mat)[1], by = 2), seq(1, dim(mat)[2], by = 2)] # bottom left
    mat3 <- mat[seq(1, dim(mat)[1], by = 2), seq(2, dim(mat)[2], by = 2)] # top right
    mat4 <- mat[seq(2, dim(mat)[1], by = 2), seq(2, dim(mat)[2], by = 2)] # bottom right

    mat <- mat1 + mat2 + mat3 + mat4

    colnames(mat) <- seq(-179.750, 179.750, by = 0.50)
    rownames(mat) <- seq(89.750, -89.750, by = -0.50)

    mat <- melt(mat) %>% rename(lat = .data$Var1, lon = .data$Var2, emission_val = .data$value)

    return(mat)
  }

  # Main body ---------------------------------------------------------------

  # Potential partitions: SAVA, BORF, TEMF, DEFO, PEAT, AGRI
  partitionsConsidered <- c("SAVA", "BORF", "TEMF", "DEFO", "PEAT", "AGRI")

  # See GFED_emissionsFactors.txt for full list of potential emissions
  emissionsConsidered <- c("CH4", "N2O", "NOx", "NH3")

  availableYears <- list.files(pattern = "\\d{4}") %>% str_extract("\\d{4}")

  d <- expand_grid(year = availableYears,
                   base_emission = "DM",
                   partition = partitionsConsidered)

  # Calculate yearly DM emissions (Mt DM per 0.25x0.25 degree grid cell)
  d <- d %>%
    mutate(DM = pmap(.l = list(.data$year, .data$base_emission, .data$partition),
                     .f = ~ .calculateYear(..1, ..2, ..3)))

  # Read in further emission factors
  emissionFactors <- read.table("GFED_emissionsFactors.txt",
                                col.names =  c("species", "SAVA", "BORF", "TEMF", "DEFO", "PEAT", "AGRI"))

  emissionFactors <- emissionFactors %>%
    filter(.data$species %in% emissionsConsidered) %>%
    pivot_longer(-.data$species, names_to = "partition", values_to = "emission_factor") %>%
    mutate(species = as.character(.data$species))

  # g X per kg X to Mt X per Mt X
  emissionFactors <- emissionFactors %>%
    mutate(emission_factor = .data$emission_factor * 1E-3)

  # Calculate derived emissions (Mt X per 0.25x0.25 degree grid cell)
  d <- inner_join(d, emissionFactors, by = "partition") %>%
    mutate(emission_val = map2(.x = .data$DM,
                               .y = .data$emission_factor,
                               .f = ~ .x * .y))

  # Convert emissions units. GFED reports NOx as NO, and MAgPIE outputs NO2.
  NOxToNO2 <- 23 / 15 # nolint

  d <- d %>%
    mutate(emission_val = ifelse(test = (.data$species == "NOx"),
                                 yes  = map(.x = .data$emission_val,
                                            .f = ~ .x * NOxToNO2),
                                 no   = .data$emission_val)) %>%
    mutate(species = ifelse(test = (.data$species == "NOx"),
                            yes  = "NO2",
                            no   = .data$species))

  # Rescale GFED data to align with MAgPIE's spatial grid (Mt X per 0.5x0.5 degree grid cell)
  d <- d %>%
    mutate(emission_val = map(.x = .data$emission_val,
                              .f = .rescaleGrid)) %>%
    select(.data$year, .data$partition, .data$species, .data$emission_val)

  # Filter to use MAgPIE's celliso format
  mapping <- toolGetMapping(name = "CountryToCellMapping.rds", where = "mstools") %>% # nolint: object_usage_linter.
    select(.data$celliso, .data$lon, .data$lat)

  d <- d %>%
    mutate(emission_val = map(.x = .data$emission_val,
                              .f = ~ inner_join(.x, mapping, by = c("lat", "lon"))))

  # Convert to MAgPIE object
  d <- d %>%
    unnest(.data$emission_val) %>%
    select(.data$celliso, .data$year, .data$partition, .data$species, .data$emission_val) %>%
    rename(Partition = .data$partition, Species = .data$species) %>%
    mutate(year = paste0("y", as.character(.data$year)))

  d <- as.magpie(d, tidy = TRUE, filter = FALSE)

  getNames(d, dim = 2) <- tolower(getNames(d, dim = 2))

  return(d)

}
