#' @title downloadNitrogenBoundariesGridded
#' @description Download he grid-level regional nitrogen boundary datasets from Schulte-Uebbing et al. (2022)
#' @author Michael S. Crawford
#'
#' @seealso [madrat::downloadSource()]
#' @examples \dontrun{
#'  a <- downloadSource("NitrogenBoundariesGridded")
#' }

downloadNitrogenBoundariesGridded <- function() {

  baseURL <- "https://zenodo.org/record/6395016"

  # download datasets
  dataURL <- paste0(baseURL, "/files/Global_critical_N_surpluses_and_N_inputs_and_their_exceedances.7z?download=1")
  archiveName <- "critical_N_surpluses_inputs_exceedances.7z"
  download.file(dataURL, destfile = archiveName)
  system(paste0("7z e ", archiveName))

  # download readme
  readmeURL <- paste0(baseURL, "/files/README.xlsx?download=1")
  download.file(readmeURL, "README.xlsx")

  # permissions
  files <- list.files(".", all.files = TRUE, full.names = TRUE, recursive = TRUE)
  Sys.chmod(paths = files, mode = "664", use_umask = FALSE)

  # nolint start
  return(list(
    url          = baseURL,
    title        = "Global spatially explicit critical nitrogen surpluses and critical nitrogen inputs, and their exceedances",
    doi          = "https://doi.org/10.1038/s41586-022-05158-2",
    revision     = "1.0",
    description  = "Input datafiles: Contains complete set of input files used in the calculations of global, spatially
                    explicit critical nitrogen surpluses and critical nitrogen inputs. All input files are output from the IMAGE-GNM model.
                    For further information on IMAGE-GNM, see:  Beusen, A. H. W., Van Beek, L. P. H., Bouwman, A. F., Mogollon, J. M.,
                    & Middelburg, J. J. (2015). Coupling global models for hydrology and nutrient loading to simulate nitrogen and
                    phosphorus retention in surface water - Description of IMAGE-GNM and analysis of performance. Geoscientific Model
                    Development, 8(12), 4045:4067. https://doi.org/10.5194/gmd-8-4045-2015. Output datafiles: Selection of output datafiles,
                    supporting results presented in the paper. For more information, see file README.xlsx.",
    unit         = "various, see README.xlsx",
    release_date = "2022-03-29",
    license      = "Creative Commons Attribution 4.0 International",
    author       = list("L.F. Schulte-Uebbing",
                        "A.H.W. Beusen",
                        "A.F. Bouwman",
                        "W. de Vries")
  ))
  # nolint end
}
