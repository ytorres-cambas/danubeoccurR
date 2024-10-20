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
