#' Download GBIF Records for Specified Species within a Polygon
#'
#' This function downloads occurrence records from GBIF for a given vector of species
#' names within a specified polygon. The function sets up GBIF credentials, checks for
#' taxon keys, handles synonyms, and ensures the polygon's coordinates are in the
#' correct winding order.
#'
#' @param species_names A vector of species names to download records for.
#' @param polygon_path A path to a GeoPackage (gpkg) file containing the polygon
#' for spatial filtering.
#' @param output_occur_path The path where the downloaded records will be saved.
#' @param gbif_user Your GBIF username.
#' @param gbif_pwd Your GBIF password.
#' @param gbif_email Your GBIF email for notifications.
#'
#' @return A data frame of occurrence records within the specified polygon.
#' @export
#'
#' @importFrom rgbif name_backbone_checklist occ_download occ_download_wait occ_download_get occ_download_import gbif_citation
#' @importFrom sf st_read st_coordinates st_polygon st_as_text st_sfc
#' @importFrom dplyr %>% filter
#'
#' @examples
#' download_gbif_records(species_names = c("Species A", "Species B"),
#'                       polygon_path = "./roi_danube.gpkg",
#'                       output_occur_path = "./downloaded_records",
#'                       gbif_user = "your_username",
#'                       gbif_pwd = "your_password",
#'                       gbif_email = "your_email@example.com")

download_gbif_records <- function(species_names, polygon_path, output_occur_path,
                                  gbif_user, gbif_pwd, gbif_email) {

  # Check that all parameters are provided
  if (missing(species_names) || !is.vector(species_names) || length(species_names) == 0) {
    stop("Error: 'species_names' must be a non-empty vector of species names.")
  }

  if (missing(polygon_path) || !file.exists(polygon_path)) {
    stop("Error: 'polygon_path' must be a valid file path to a GeoPackage (gpkg) file.")
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

  # 2. Import polygon
  polygon_sf <- sf::st_read(polygon_path)

  # 3. Get WKT representation (ensure counterclockwise winding order)
  vertices <- sf::st_coordinates(polygon_sf)[, 1:2]

  # Function to calculate the signed area of the polygon
  calculate_signed_area <- function(vertices) {
    n <- nrow(vertices)
    sum <- 0
    for (i in 1:(n-1)) {
      sum <- sum + (vertices[i, 1] * vertices[i + 1, 2] - vertices[i + 1, 1] * vertices[i, 2])
    }
    sum <- sum + (vertices[n, 1] * vertices[1, 2] - vertices[1, 1] * vertices[n, 2])
    return(sum / 2)
  }

  # Calculate the signed area before transformation
  signed_area_before <- calculate_signed_area(vertices)

  # Reverse the order of vertices to transform to counterclockwise
  vertices_reversed <- vertices[nrow(vertices):1, ]

  # Calculate the signed area after transformation
  signed_area_after <- calculate_signed_area(vertices_reversed)

  # Create new polygon with reversed vertices
  polygon_reversed_sf <- sf::st_polygon(list(vertices_reversed))
  wkt <- sf::st_as_text(sf::st_sfc(polygon_reversed_sf))

  # 4. Download occurrences
  gbif_download <- rgbif::occ_download(
    rgbif::pred_and(
      rgbif::pred("HAS_GEOSPATIAL_ISSUE", FALSE),
      rgbif::pred("HAS_COORDINATE", TRUE),
      rgbif::pred_not(rgbif::pred_in("BASIS_OF_RECORD", c("FOSSIL_SPECIMEN", "LIVING_SPECIMEN")))
    ),
    rgbif::pred_in("taxonKey", taxonkey_table$usageKey),
    rgbif::pred_within(wkt),
    format = "SIMPLE_CSV"
  )

  # Check if download is finished
  rgbif::occ_download_wait(gbif_download)

  # Retrieve and load the download
  raw_downloaded <- rgbif::occ_download_get(gbif_download, path = output_occur_path) %>%
    rgbif::occ_download_import()

  # 5. Get and save citation
  rgbif::gbif_citation(gbif_download[1])$download %>%
    writeLines(paste0(output_occur_path, "/gbif_download_citation_", Sys.Date(), ".txt"))

  return(raw_downloaded)
}
