#' Extract Municipality and Province using Latitude and Longitude
#'
#' This function takes latitude and longitude values and returns the
#' corresponding municipality and province (Darwin Core standard) using reverse geocoding.
#' Supports both OpenStreetMap (OSM) and Google Maps API.
#'
#' @param latitude Numeric, latitude of the point.
#' @param longitude Numeric, longitude of the point.
#' @param service Character, geocoding service to use. Options are "osm" (OpenStreetMap) or "google". Default is "osm".
#' @param api_key Character, the Google Maps API key (required if using Google Maps).
#' @return A list with municipality and province.
#'
#' @importFrom tidygeocoder reverse_geocode
#' @import dplyr
#' @examples
#' \dontrun{
#' # Example usage with OpenStreetMap:
#' # Point in Vienna, Austria (Danube River)
#' result_vienna <- extract_municipality_province(48.2082, 16.3738, service = "osm")
#' print(result_vienna)
#'
#' # Point in Belgrade, Serbia (Danube River)
#' result_belgrade <- extract_municipality_province(44.7866, 20.4489, service = "osm")
#' print(result_belgrade)
#'
#' # Example usage with Google Maps API (replace "your_api_key_here" with your actual API key):
#' result_vienna_google <- extract_municipality_province(48.2082, 16.3738, service = "google", api_key = "your_api_key_here")
#' print(result_vienna_google)
#'
#' result_belgrade_google <- extract_municipality_province(44.7866, 20.4489, service = "google", api_key = "your_api_key_here")
#' print(result_belgrade_google)
#' }
#' @export
extract_municipality_province <- function(latitude, longitude, service = "osm", api_key = NULL) {

  # Check if latitude and longitude are numeric
  if (!is.numeric(latitude) | !is.numeric(longitude)) {
    stop("Latitude and longitude must be numeric.")
  }

  # Ensure Google API key is provided if using Google Maps
  if (service == "google" && is.null(api_key)) {
    stop("Google Maps API key must be provided for the Google service.")
  }

  # Perform reverse geocoding using the specified service
  if (service == "osm") {
    # Using OpenStreetMap for reverse geocoding
    location_info <- tidygeocoder::reverse_geocode(
      lat = latitude,
      long = longitude,
      method = "osm"
    )
  } else if (service == "google") {
    # Using Google Maps for reverse geocoding
    location_info <- tidygeocoder::reverse_geocode(
      lat = latitude,
      long = longitude,
      method = "google",
      full_results = TRUE,
      api_key = api_key
    )
  } else {
    stop("Unsupported service. Use 'osm' or 'google'.")
  }

  # Extract municipality and province/state information
  if (service == "osm") {
    location_info <- location_info %>%
      dplyr::select(municipality = address_municipality, province = address_state) %>%
      dplyr::slice(1) # Take the first result

  } else if (service == "google") {
    # Google Maps API returns more detailed results
    address_components <- location_info$address_components[[1]]

    # Extract municipality and province from Google API response
    municipality <- address_components %>%
      purrr::keep(~ "locality" %in% .x$types) %>%
      purrr::pluck(1, "long_name", .default = NA)

    province <- address_components %>%
      purrr::keep(~ "administrative_area_level_1" %in% .x$types) %>%
      purrr::pluck(1, "long_name", .default = NA)

    location_info <- list(municipality = municipality, province = province)
  }

  return(as.list(location_info))
}
