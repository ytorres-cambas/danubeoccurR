#' Visualize Points on a Map with Optional Layers and Metadata
#'
#' This function takes a data frame containing latitude and longitude coordinates
#' (default columns: 'decimalLatitude' and 'decimalLongitude') and optionally additional
#' columns (e.g., species, locality, year, dataset name). It visualizes the points
#' on a map using the `leaflet` package, and the popup will show the coordinates along
#' with any other provided information if the user chooses to display it. Additionally,
#' it allows loading a vector or raster layer to the map, and the points will be displayed
#' on top of this layer.
#'
#' @param points_df A data frame containing columns named 'decimalLatitude' and 'decimalLongitude'.
#' Optionally, the data frame can include other columns such as species, locality, year,
#' dataset name, or any additional metadata.
#' @param latitude_col The name of the latitude column in `points_df`. Default is 'decimalLatitude'.
#' @param longitude_col The name of the longitude column in `points_df`. Default is 'decimalLongitude'.
#' @param show_extra_columns A logical value indicating whether to display extra columns
#' (e.g., species, locality, year) in the popup. Default is FALSE.
#' @param layer An optional vector or raster layer (e.g., an sf object or a raster object)
#' to be added to the map. The points will be plotted on top of this layer.
#'
#' @return A `leaflet` map object with the points plotted on it, and optionally, an additional
#' vector or raster layer. The popup will display the coordinates and, if `show_extra_columns` is TRUE,
#' any additional columns provided.
#' @import leaflet
#' @importFrom leaflet addTiles addCircleMarkers addPolygons addRasterImage
#' @examples
#' # Example: Visualize locations along the Danube River with fish species data
#' fish_data_2 <- fish_data %>%
#' select(species, decimalLatitude, decimalLongitude)
#' visualize_points(fish_data_2, show_extra_columns = TRUE)
#'
#' # Example with additional vector layer
#' visualize_points(fish_data_2, layer = danube_basin, show_extra_columns = TRUE)
#'
#' @export
visualize_points <- function(points_df,
                             latitude_col = "decimalLatitude",
                             longitude_col = "decimalLongitude",
                             show_extra_columns = FALSE,
                             layer = NULL) {

  # Check if latitude and longitude columns are present
  if (!all(c(latitude_col, longitude_col) %in% colnames(points_df))) {
    stop("The data frame must contain latitude and longitude columns.")
  }

  # Base popup content (coordinates only)
  popup_content <- paste(
    "Latitude:", points_df[[latitude_col]],
    "<br>Longitude:", points_df[[longitude_col]]
  )

  # If the user chooses to show extra columns
  if (show_extra_columns) {
    additional_columns <- setdiff(colnames(points_df), c(latitude_col, longitude_col))

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
  map <- leaflet::leaflet() %>%
    leaflet::addTiles()  # Add default OpenStreetMap tiles

  # Add the optional layer (vector or raster)
  if (!is.null(layer)) {
    if (inherits(layer, "sf")) {
      map <- map %>%
        leaflet::addPolygons(data = layer, color = "gray", fillOpacity = 0.3)
    } else if (inherits(layer, "RasterLayer")) {
      map <- map %>%
        leaflet::addRasterImage(layer, opacity = 0.5)
    } else {
      stop("Layer must be either an sf object (vector) or a RasterLayer object.")
    }
  }

  # Add the points on top of the layers
  map <- map %>%
    leaflet::addCircleMarkers(
      lng = points_df[[longitude_col]],
      lat = points_df[[latitude_col]],
      radius = 5,
      color = "blue",
      fillOpacity = 0.7,
      popup = popup_content
    )

  return(map)
}
