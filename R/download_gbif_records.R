#' Download GBIF Records for Specified Species within a Polygon
#'
#' This function downloads occurrence records from GBIF for a given vector of species
#' names within a specified polygon, provided as a WKT string. The function sets up
#' GBIF credentials, checks for taxon keys, handles synonyms, and downloads records
#' based on the polygon and species names.
#'
#' The function automatically filters records to ensure that occurrences with geospatial
#' issues or missing coordinates are excluded. Additionally, it excludes fossil and
#' living specimens from the download.
#'
#' @param species_names A vector of species names to download records for.
#' @param wkt The well-known text (WKT) representation of a polygon for spatial filtering.
#' @param output_occur_path The path where the downloaded records will be saved.
#' @param gbif_user Your GBIF username.
#' @param gbif_pwd Your GBIF password.
#' @param gbif_email Your GBIF email for notifications.
#' @param import_to_r A logical value indicating whether to import the downloaded data into R. Default is TRUE.
#'
#' @return A list containing the downloaded occurrence records (if `import_to_r` is TRUE)
#'         and the citation for the GBIF download.
#' @export
#'
#' @importFrom rgbif name_backbone_checklist occ_download occ_download_wait occ_download_get occ_download_import gbif_citation
#' @import  dplyr
#'
#' @examples
#' # Define the path to the polygon file included in the package
#' polygon_path <- system.file("extdata", "danube_basin.gpkg", package = "yourPackageName")
#'
#' # Convert the polygon to WKT format using the `get_wkt` function from your package
#' wkt <- get_wkt(bbox_danube_basin)
#'
#' # Download occurrences for species of interest
#' output_occur_path <- "./downloaded_records"
#' gbif_data <- download_gbif_records(
#'   species_names = c("Hucho hucho", "Alburnoides bipunctatus", "Chondrostoma nasus"),
#'   wkt = wkt,
#'   output_occur_path = output_occur_path,
#'   gbif_user = "your_username",
#'   gbif_pwd = "your_password",
#'   gbif_email = "your_email@example.com",
#'   import_to_r = TRUE
#' )
#'
#' # Access the downloaded records
#' occurrence_records <- gbif_data$raw_download
#'
#' # Access the GBIF citation
#' gbif_citation <- gbif_data$gbif_download_citation
#'
#' # Create a metadata file for the downloaded records
#' create_metadata_file(file_path = paste0(output_occur_path,"file_name.zip"),
#' description = "This file contains fish species occurrences from GBIF",
#' author = "Your Name",
#' author_email = "your_email@example.com",
#' source = gbif_citation)

download_gbif_records <- function(species_names, wkt, output_occur_path,
                                  gbif_user, gbif_pwd, gbif_email, import_to_r = TRUE) {

  # Check that all parameters are provided
  if (missing(species_names) || !is.vector(species_names) || length(species_names) == 0) {
    stop("Error: 'species_names' must be a non-empty vector of species names.")
  }

  if (missing(wkt) || !is.character(wkt) || nchar(wkt) == 0) {
    stop("Error: 'wkt' must be a valid WKT string.")
  }

  if (missing(output_occur_path)) {
    stop("Error: 'output_occur_path' must be specified to save the downloaded records.")
  }

  if (missing(gbif_user) || !is.character(gbif_user) || nchar(gbif_user) == 0) {
    stop("Error: 'gbif_user' must be a non-empty string.")
  }

  if (missing(gbif_pwd) || !is.character(gbif_pwd) || nchar(gbif_pwd) == 0) {
    stop("Error: 'gbif_pwd' must be a non-empty string.")
  }

  if (missing(gbif_email) || !is.character(gbif_email) || nchar(gbif_email) == 0) {
    stop("Error: 'gbif_email' must be a non-empty string.")
  }

  # Set up GBIF credentials
  Sys.setenv(GBIF_USER = gbif_user, GBIF_PWD = gbif_pwd, GBIF_EMAIL = gbif_email)

  # 1. Get keys for each species
  taxonkey_table <- rgbif::name_backbone_checklist(species_names)

  # Check for missing taxon keys
  sp_miss_tk <- species_names[is.na(taxonkey_table$usageKey)]
  if (length(sp_miss_tk) > 0) {
    cat(paste0("No taxon key for: ", paste(sp_miss_tk, collapse = ", "), "\n"))

    # Try with synonyms
    synom_sp <- sapply(sp_miss_tk, function(sp) {
      temp <- taxonkey_table[taxonkey_table$species == sp, "synonym"]
      ifelse(length(temp) > 0, temp, NA)
    })
    sp_list <- c(species_names[!species_names %in% sp_miss_tk], na.omit(synom_sp))

    # Get taxon keys for the updated species list
    taxonkey_table <- rgbif::name_backbone_checklist(sp_list)
  }

  # 2. Download occurrences
  gbif_download <- rgbif::occ_download(
    rgbif::pred_and(
      rgbif::pred("HAS_GEOSPATIAL_ISSUE", FALSE),  # Exclude records with geospatial issues
      rgbif::pred("HAS_COORDINATE", TRUE),  # Include only records with coordinates
      rgbif::pred_not(rgbif::pred_in("BASIS_OF_RECORD", c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN")))  # Exclude fossil and living specimens
    ),
    rgbif::pred_in("taxonKey", taxonkey_table$usageKey),
    rgbif::pred_within(wkt),
    format = "SIMPLE_CSV"
  )

  # Check if download is finished
  rgbif::occ_download_wait(gbif_download)

  # Retrieve the downloaded data
  rgbif::occ_download_get(gbif_download, path = output_occur_path)

  # If import_to_r is TRUE, load the downloaded data into R
  if (import_to_r) {
    raw_downloaded <- rgbif::occ_download_get(gbif_download,
                                              path = output_occur_path) |>
      rgbif::occ_download_import(output_occur_path)
  } else {
    raw_downloaded <- NULL
  }

  # 3. Get and save citation
  gbif_citation <- rgbif::gbif_citation(gbif_download[1])$download

  return(list(raw_download = raw_downloaded, gbif_download_citation = gbif_citation))
}
