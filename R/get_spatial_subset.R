#' Spatial Subset of Coordinates within a Polygon
#'
#' This function takes a polygon (`sf` object) and a data frame with latitude and
#' longitude columns, and filters the data frame to retain only the coordinates
#' that fall within the polygon. The CRS (Coordinate Reference System) of both
#' the polygon and the coordinates must match, and the default CRS is WGS84.
#'
#' @param polygon An `sf` object representing the polygon for spatial filtering.
#' @param data A data frame containing at least two columns for latitude and longitude.
#' @param lat_col The name of the column in `data` representing latitude.
#' @param lon_col The name of the column in `data` representing longitude.
#' @param crs The CRS (Coordinate Reference System) to ensure consistency between the polygon and coordinates. Default is WGS84 (`EPSG:4326`).
#' @param verbose Logical. If `TRUE`, prints detailed messages. Default is `FALSE`.
#'
#' @return A data frame containing only the rows of the original data frame where
#' the coordinates fall within the polygon.
#'
#' @importFrom sf st_as_sf st_transform st_crs st_within
#' @importFrom dplyr filter
#'
#' @export
#'
#' @examples
#' # Example polygon and data
#' polygon <- danube_basin
#' data <- fish_data
#' filtered_data <- get_spatial_subset(polygon,
#'                                     data,
#'                                     "decimalLatitude",
#'                                     "decimalLongitude",
#'                                     verbose = TRUE)
#'
#' # Map with points that fall within the polygon
#'

get_spatial_subset <- function(polygon, data, lat_col, lon_col, crs = 4326, verbose = FALSE) {

  # Check if the input is a data frame
  if (!is.data.frame(data)) {
    stop("The 'data' parameter must be a data frame.")
  }

  # Check if latitude and longitude columns exist in the data frame
  if (!lat_col %in% colnames(data)) {
    stop(paste("The latitude column", lat_col, "is not present in the data frame."))
  }

  if (!lon_col %in% colnames(data)) {
    stop(paste("The longitude column", lon_col, "is not present in the data frame."))
  }

  # Check if the polygon is an sf object
  if (!inherits(polygon, "sf")) {
    stop("The 'polygon' parameter must be an 'sf' object.")
  }

  # Convert the data frame into an sf object using the latitude and longitude columns
  points_sf <- sf::st_as_sf(data, coords = c(lon_col, lat_col), crs = crs)

  # Check if the CRS of the points and the polygon match
  if (sf::st_crs(points_sf) != sf::st_crs(polygon)) {
    if (verbose) {
      message("CRS mismatch: Transforming the polygon to match the CRS of the data.")
    }
    polygon <- sf::st_transform(polygon, crs = sf::st_crs(points_sf))
  }

  # Perform spatial filtering (points within polygon)
  points_within <- sf::st_within(points_sf, polygon, sparse = FALSE)

  # Filter the data frame for points that are within the polygon
  filtered_data <- data[points_within, ]

  if (verbose) {
    message(paste(nrow(filtered_data), "out of", nrow(data), "points are within the polygon."))
  }

  return(filtered_data)
}
