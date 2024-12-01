#' Perform a Spatial Join of Points with GADM Administrative Boundaries
#'
#' This function downloads GADM administrative boundary data for specified countries
#' and performs a spatial join to add administrative information to a dataset containing
#' geographic coordinates. Optionally, an `sf` object can be provided instead of downloading data.
#'
#' @param country_codes A character vector of ISO3 country codes for which GADM data will be downloaded.
#'   Defaults to the countries in the Danube River Basin.
#' @param coords_df A data frame containing coordinates to spatially join with GADM administrative data.
#'   It must include columns for latitude and longitude.
#' @param lat_col The name of the column in `coords_df` containing latitude values. Defaults to "decimalLatitude".
#' @param lon_col The name of the column in `coords_df` containing longitude values. Defaults to "decimalLongitude".
#' @param sf_gadm Optional `sf` object containing GADM administrative boundaries. If provided, it is used
#'   directly for the spatial join. Defaults to `NULL`.
#' @param timeout Timeout in seconds for downloads. Defaults to 600 (10 minutes).
#'
#' @return A data frame with the same columns as the input `coords_df`, along with administrative attributes
#'   from the GADM data.
#'
#' @importFrom sf st_as_sf st_read st_join st_within st_drop_geometry st_coordinates
#' @importFrom dplyr mutate
#' @export

spatial_join_gadm <- function(
    country_codes = c("AUT", "BIH", "BGR", "CHE", "CZE", "DEU", "HUN", "HRV",
                      "MDA", "MNE", "POL", "ROU", "SRB", "SVK", "SVN", "UKR"),
    coords_df,
    lat_col = "decimalLatitude",
    lon_col = "decimalLongitude",
    sf_gadm = NULL,
    timeout = 600
) {
  # Check if coordinate columns exist in the input data frame
  if (!all(c(lon_col, lat_col) %in% colnames(coords_df))) {
    stop(paste("The data frame must contain columns:", lon_col, "and", lat_col))
  }

  # Convert input data frame to an sf object
  points_sf <- st_as_sf(
    coords_df,
    coords = c(lon_col, lat_col),
    crs = 4326
  )

  if (!is.null(sf_gadm)) {
    # Use the provided sf object for the spatial join
    message("Using provided sf object for GADM boundaries.")
    joined_sf <- st_join(points_sf, sf_gadm, join = st_within)
  } else {
    # Create a temporary directory for GADM files
    temp_dir <- tempfile(pattern = "gadm_temp")
    dir.create(temp_dir)

    options(timeout = timeout)

    # Initialize a list to store administrative sf objects
    admin_sf_list <- list()

    tryCatch({
      for (country_code in country_codes) {
        gadm_url <- paste0(
          "https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_",
          country_code,
          ".gpkg"
        )
        gadm_file <- gsub("\\\\", "/", file.path(temp_dir, paste0("gadm41_", country_code, ".gpkg")))
        download.file(gadm_url, gadm_file, mode = "wb")
        message(paste("Downloaded GADM file for", country_code))

        layers <- st_layers(gadm_file)$name
        layer <- if ("ADM_ADM_2" %in% layers) {
          st_read(gadm_file, layer = "ADM_ADM_2", quiet = TRUE)
        } else if ("ADM_ADM_1" %in% layers) {
          st_read(gadm_file, layer = "ADM_ADM_1", quiet = TRUE)
        } else if ("ADM_ADM_0" %in% layers) {
          st_read(gadm_file, layer = "ADM_ADM_0", quiet = TRUE)
        } else {
          warning(paste("No suitable administrative layer found for", country_code))
          NULL
        }

        if (!is.null(layer)) {
          admin_sf_list[[country_code]] <- layer
        }
      }

      if (length(admin_sf_list) == 0) {
        stop("No valid administrative layers found for any of the countries.")
      }

      merged_admin_sf <- do.call(rbind, admin_sf_list)
      joined_sf <- st_join(points_sf, merged_admin_sf, join = st_within)
    }, finally = {
      unlink(temp_dir, recursive = TRUE)
      message("Temporary files deleted.")
    })
  }

  # Convert back to data frame
  result <- joined_sf %>%
    st_drop_geometry() %>%
    mutate(
      !!lon_col := st_coordinates(points_sf)[, "X"],
      !!lat_col := st_coordinates(points_sf)[, "Y"]
    )

  return(result)
}
