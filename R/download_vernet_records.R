#' Download Occurrence Records from Vernet
#'
#' This function downloads occurrence records from Vernet for a given vector of species
#' names within a specified polygon. The function sets up the necessary parameters and
#' handles any potential issues with the input. It also saves metadata about the download.
#'
#' @param species_names A vector of species names to download records for.
#' @param polygon_path A path to a GeoPackage (gpkg) file containing the polygon
#' for spatial filtering.
#' @param output_occur_path The path where the downloaded records will be saved.
#' @param metadata_description An optional description for the metadata file.
#'
#' @return A data frame of occurrence records within the specified polygon.
#' @export
#'
#' @importFrom rvertnet searchbyterm
#' @importFrom sf st_read
#'
#' @examples
#' download_vernet_records(species_names = c("Species A", "Species B"),
#'                         polygon_path = "./roi_danube.gpkg",
#'                         output_occur_path = "./downloaded_records",
#'                         metadata_description = "Initial download of species occurrences.")

download_vernet_records <- function(species_names, polygon_path, output_occur_path, metadata_description = "") {

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

  # 1. Import polygon
  polygon_sf <- sf::st_read(polygon_path)

  # 2. Download occurrences
  occurrences <- rvertnet::searchbyterm(
    species = species_names,
    polygon = polygon_sf
  )

  # 3. Check if any occurrences were returned
  if (nrow(occurrences) == 0) {
    warning("No occurrences found for the specified species and polygon.")
  }

  # 4. Save the occurrences to the specified output path
  csv_filename <- paste0("vernet_occurrences_", Sys.Date(), ".csv")
  write.csv(occurrences, file = file.path(output_occur_path, csv_filename), row.names = FALSE)

  # 5. Create metadata
  metadata <- data.frame(
    download_date = Sys.Date(),
    csv_file_name = csv_filename,
    output_path = output_occur_path,
    description = metadata_description,
    stringsAsFactors = FALSE
  )

  # 6. Save metadata as a text file
  metadata_filename <- paste0("metadata_", Sys.Date(), ".txt")
  writeLines(c(
    paste("Download Date:", Sys.Date()),
    paste("CSV File Name:", csv_filename),
    paste("Output Path:", output_occur_path),
    paste("Description:", metadata_description)
  ), con = file.path(output_occur_path, metadata_filename))

  return(occurrences)
}
