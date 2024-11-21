#' Get Spatial Subset of Data
#'
#' Filters a data frame of spatial points to include only those within a specified polygon.
#'
#' @param polygon An `sf` object representing the polygon to use for filtering.
#' @param data A data frame containing spatial point data with latitude and longitude columns.
#' @param lat_col A character string specifying the name of the latitude column in the data frame.
#' @param lon_col A character string specifying the name of the longitude column in the data frame.
#' @param crs An integer specifying the Coordinate Reference System (CRS) for the input points (default is 4326).
#' @param verbose A logical value indicating whether to display messages about processing (default is `FALSE`).
#'
#' @return A filtered data frame containing only the points within the specified polygon.
#'
#' @details
#' This function converts a data frame with latitude and longitude columns into an `sf` object, checks for CRS consistency
#' between the input polygon and the data, and performs a spatial filtering operation to retain only the points
#' that fall within the given polygon.
#'
#' If the CRS of the input polygon does not match the CRS of the data, the polygon is transformed to match the data's CRS.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Example polygon
#' polygon <- st_as_sf(data.frame(
#'   id = 1,
#'   geometry = st_sfc(st_polygon(list(matrix(c(
#'     -10, -10,
#'     10, -10,
#'     10, 10,
#'     -10, 10,
#'     -10, -10
#'   ), ncol = 2, byrow = TRUE))))
#' ), crs = 4326)
#'
#' # Example data frame
#' data <- data.frame(
#'   id = 1:5,
#'   lat = c(0, 5, -15, 20, -5),
#'   lon = c(0, 5, 15, -20, -5)
#' )
#'
#' # Subset points within the polygon
#' filtered_data <- get_spatial_subset(polygon, data, lat_col = "lat", lon_col = "lon", verbose = TRUE)
#' print(filtered_data)
#' }
#'
#' @importFrom sf st_as_sf st_within st_transform st_crs
#' @export


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

  # Convert the matrix result into a logical vector
  points_within_logical <- apply(points_within, 1, any)

  # Filter the data frame for points that are within the polygon
  filtered_data <- data[points_within_logical, ]

  if (verbose) {
    message(paste(sum(points_within_logical), "out of", nrow(data), "points are within the polygon."))
  }

  return(filtered_data)
}
