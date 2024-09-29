#' Visualize Points on a Map with Optional Metadata
#'
#' This function takes a data frame containing latitude and longitude coordinates
#' and optionally additional columns (e.g., species, locality, year, dataset name).
#' It visualizes the points on a map using the `leaflet` package, and the popup
#' will show the coordinates along with any other provided information if the user
#' chooses to display it.
#'
#' @param points_df A data frame containing columns named 'latitude' and 'longitude'.
#' Optionally, the data frame can include other columns such as species, locality, year,
#' dataset name, or any additional metadata.
#' @param show_extra_columns A logical value indicating whether to display extra columns
#' (e.g., species, locality, year) in the popup. Default is FALSE.
#'
#' @return A `leaflet` map object with the points plotted on it. The popup will display
#' the coordinates and, if `show_extra_columns` is TRUE, any additional columns provided.
#' @import leaflet
#' @examples
#' # Example: Visualize locations along the Danube River with fish species data
#' danube_points <- data.frame(
#'   latitude = c(48.2082, 47.4979, 45.2671, 44.8176),  # Vienna, Budapest, Novi Sad, Belgrade
#'   longitude = c(16.3738, 19.0402, 19.8335, 20.4633),
#'   species = c("Hucho hucho", "Alburnus alburnus", "Perca fluviatilis", "Acipenser ruthenus"),
#'   locality = c("Vienna", "Budapest", "Novi Sad", "Belgrade"),
#'   year = c(2020, 2019, 2021, 2022),
#'   dataset = c("Dataset 1", "Dataset 2", "Dataset 3", "Dataset 4")
#' )
#' visualize_points(danube_points, show_extra_columns = TRUE)
#'
#' @export
visualize_points <- function(points_df, show_extra_columns = FALSE) {
  # Check if latitude and longitude columns are present
  if (!all(c("latitude", "longitude") %in% colnames(points_df))) {
    stop("The data frame must contain 'latitude' and 'longitude' columns.")
  }

  # Base popup content (coordinates only)
  popup_content <- paste(
    "Latitude:", points_df$latitude,
    "<br>Longitude:", points_df$longitude
  )

  # If the user chooses to show extra columns
  if (show_extra_columns) {
    additional_columns <- setdiff(colnames(points_df), c("latitude", "longitude"))

    if (length(additional_columns) > 0) {
      for (col in additional_columns) {
        popup_content <- paste(
          popup_content,
          paste0("<br>", col, ": ", points_df[[col]])
        )
      }
    }
  }

  # Create the leaflet map
  map <- leaflet::leaflet(data = points_df) %>%
    leaflet::addTiles() %>%  # Add default OpenStreetMap tiles
    leaflet::addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = 5,
      color = "blue",
      fillOpacity = 0.7,
      popup = popup_content
    )

  return(map)
}
