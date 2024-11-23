#' Extract Municipality, Province, and Country using Latitude and Longitude
#'
#' This function takes latitude and longitude values and returns the
#' corresponding municipality, province, and country (Darwin Core standard) using reverse geocoding.
#' Supports both OpenStreetMap (OSM) and Google Maps API.
#'
#' @param data A data frame containing the latitude and longitude columns.
#' @param latitude Name of the column for latitude in the `data` frame (as a string).
#' @param longitude Name of the column for longitude in the `data` frame (as a string).
#' @param service Character, geocoding service to use. Options are "osm" (OpenStreetMap) or "google". Default is "osm".
#' @param api_key Character, the Google Maps API key (required if using Google Maps).
#' @return A data frame with additional columns for municipality, province, and country.
#'
#' @importFrom tidygeocoder reverse_geocode
#' @import dplyr
#' @import purrr
#' @import rlang
#' @examples
#' \dontrun{
#' # Example usage with OpenStreetMap:
#' # Data frame with points in Vienna and Belgrade
#' points <- data.frame(
#'   decimalLatitude = c(48.2082, 44.7866),
#'   decimalLongitude = c(16.3738, 20.4489)
#' )
#'
#' results <- extract_location_info(points, latitude = "decimalLatitude", longitude = "decimalLongitude", service = "osm")
#' print(results)
#' }
#' @export
extract_location_info <- function(data, latitude = "latitude", longitude = "longitude", service = "osm", api_key = NULL) {
  # Check if the input data frame has the required columns
  if (!(latitude %in% names(data)) | !(longitude %in% names(data))) {
    stop(paste("Specified latitude and longitude columns ('", latitude, "' and '", longitude, "') do not exist in the data frame.", sep = ""))
  }

  # Ensure Google API key is provided if using Google Maps
  if (service == "google" && is.null(api_key)) {
    stop("Google Maps API key must be provided for the Google service.")
  }

  # Perform reverse geocoding using the specified service
  if (service == "osm") {
    # Using OpenStreetMap for reverse geocoding
    location_info <- tidygeocoder::reverse_geocode(
      .tbl = data,
      lat = latitude,  # Dynamically select the latitude column
      long = longitude,  # Dynamically select the longitude column
      method = "osm",
      full_results = TRUE
    ) %>%
      select(
        county,
        state,
        country
      )
  } else if (service == "google") {
    # Using Google Maps for reverse geocoding
    location_info <- tidygeocoder::reverse_geocode(
      .tbl = data,
      lat = latitude,  # Use column names directly
      long = longitude,  # Use column names directly
      method = "google",
      full_results = TRUE,
      api_key = api_key,
      language = "en"  # Set the language to English
    )

    # Extract address components from Google API response
    location_info <- location_info %>%
      mutate(
        municipality = purrr::map_chr(
          address_components, ~ purrr::pluck(
            purrr::keep(.x, ~ "locality" %in% .x$types),
            1, "long_name", .default = NA
          )
        ),
        province = purrr::map_chr(
          address_components, ~ purrr::pluck(
            purrr::keep(.x, ~ "administrative_area_level_1" %in% .x$types),
            1, "long_name", .default = NA
          )
        ),
        country = purrr::map_chr(
          address_components, ~ purrr::pluck(
            purrr::keep(.x, ~ "country" %in% .x$types),
            1, "long_name", .default = NA
          )
        )
      )
  } else {
    stop("Unsupported service. Use 'osm' or 'google'.")
  }

  return(location_info)
}
