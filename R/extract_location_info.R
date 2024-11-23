#' Extract Municipality, Province, and Country using Latitude and Longitude (OSM)
#'
#' This function takes latitude and longitude values and returns the
#' corresponding municipality, province, and full country name (Darwin Core standard) using OpenStreetMap (OSM) for reverse geocoding.
#'
#' @param data A data frame containing the latitude and longitude columns.
#' @param latitude Name of the column for latitude in the `data` frame (as a string). Default is
#' "latitude".
#' @param longitude Name of the column for longitude in the `data` frame (as a string). Default is
#' "longitude".
#' @return A data frame with additional columns for municipality, province, and country, preserving the original rows.
#'
#' @importFrom tidygeocoder reverse_geocode
#' @importFrom countrycode countrycode
#' @import dplyr
#' @examples
#' \dontrun{
#' points <- data.frame(
#'   decimalLatitude = c(48.2082, 44.7866, 48.2082),
#'   decimalLongitude = c(16.3738, 20.4489, 16.3738)
#' )
#' results <- extract_location_info(points, latitude = "decimalLatitude", longitude = "decimalLongitude")
#' print(results)
#' }
#' @export
extract_location_info <- function(data, latitude = "latitude", longitude = "longitude") {
  # Check if the input data frame has the required columns
  if (!latitude %in% names(data) | !longitude %in% names(data)) {
    stop("Specified latitude and longitude columns do not exist in the data frame.")
  }

  # Group by latitude and longitude and assign a unique ID to each group
  data <- data %>%
    mutate(location_id = group_indices(., .data[[latitude]], .data[[longitude]]))

  # Extract unique latitude and longitude combinations
  unique_locations <- data %>%
    distinct(location_id, .keep_all = TRUE) %>%
    select(location_id, latitude = .data[[latitude]], longitude = .data[[longitude]])

  # Perform reverse geocoding for unique locations using OSM
  location_info <- tidygeocoder::reverse_geocode(
    .tbl = unique_locations,
    lat = latitude,
    long = longitude,
    method = "osm",
    full_results = TRUE
  ) %>%
    mutate(
      country = countrycode(toupper(country_code), origin = "iso2c", destination = "country.name")
    ) %>%
    select(location_id,
           municipality = county,
           province = state,
           country)

  # Merge the geocoding results back to the original data
  result <- data %>%
    left_join(location_info, by = "location_id") %>%
    select(-location_id)  # Remove the temporary ID column

  return(result)
}
