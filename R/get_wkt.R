#' Get WKT Representation of Polygon from sf Object in Counterclockwise Order
#'
#' This function takes an sf polygon object, ensures that the polygon is in
#' counterclockwise winding order, and returns the WKT (Well-Known Text)
#' representation of the polygon.
#'
#' @param sf_obj An sf object containing a polygon geometry.
#' @return A WKT string representing the polygon in counterclockwise winding order.
#' @importFrom sf st_geometry st_polygon st_sfc st_as_text
#' @export
#'
#' @examples
#' get_wkt(bbox_danube_basin)
get_wkt <- function(sf_obj) {
  # Check if the input is an sf object
  if (!inherits(sf_obj, "sf")) {
    stop("Input must be an sf object containing a polygon.")
  }

  # Extract the geometry (assuming a single polygon)
  geometry <- sf::st_geometry(sf_obj)

  # # Ensure the geometry is of type POLYGON
  if (!inherits(geometry, "sfc_POLYGON")) {
    stop("The sf object must contain a polygon geometry.")
  }

  # Function to calculate the signed area of the polygon (shoelace theorem)
  signed_area <- function(coords) {
    x <- coords[, 1]
    y <- coords[, 2]
    return(sum(x[-1] * y[-nrow(coords)] - x[-nrow(coords)] * y[-1]) / 2)
  }

  # Loop through polygons and ensure counterclockwise order for each ring
  corrected_polygons <- lapply(geometry, function(polygon) {
    coords <- polygon[[1]]  # Extract the exterior ring

    # Ensure the polygon is closed (first and last point are the same)
    if (!all(coords[1, ] == coords[nrow(coords), ])) {
      coords <- rbind(coords, coords[1, ])
    }

    # Check if the polygon is in clockwise order (signed area < 0)
    area <- signed_area(coords)
    if (area > 0) {
      # Reverse the order of the coordinates to make them counterclockwise
      coords <- coords[nrow(coords):1, ]
    }

    # Return corrected polygon
    sf::st_polygon(list(coords))
  })

  # Combine the corrected polygons into an sf object and return the WKT
  corrected_sfc <- sf::st_sfc(corrected_polygons)
  wkt <- sf::st_as_text(corrected_sfc)

  return(wkt)
}
