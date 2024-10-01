#' Convert Degrees, Minutes, Seconds (DMS) to Decimal Degrees (DD)
#'
#' This function converts geographic coordinates from Degrees, Minutes, and Seconds (DMS)
#' format to Decimal Degrees (DD). The input can be either a data frame with separate columns
#' for degrees, minutes, and seconds, or a single column with DMS represented by symbols (e.g., 34°30'45"N).
#'
#' @param data A data frame containing either separate columns for degrees, minutes, and seconds,
#' or a single column with DMS formatted values for latitude and longitude.
#' @param lat_col The name of the column representing latitude or vector. It can either be a single column with DMS notation or separate columns for degrees.
#' @param lon_col The name of the column representing longitude. It can either be a single column with DMS notation or separate columns for degrees.
#' @param is_dms_symbol If TRUE (default), assumes the latitude and longitude columns are in DMS notation with symbols (e.g., `34°30'45"N`).
#' @param verbose Logical. If `TRUE`, prints messages about the conversion process. Default is `FALSE`.
#' @return A data frame with two new columns: `lat_dd` for latitude in decimal degrees and `lon_dd` for longitude in decimal degrees.
#' @export
#' @examples
#' # Example with DMS symbol notation
#' data <- data.frame(
#'   latitude = c("34°30'00\"N", "40°45'00\"N"),
#'   longitude = c("118°15'00\"W", "73°58'00\"W")
#' )
#' result <- dms_to_decimal(data, "latitude", "longitude", is_dms_symbol = TRUE)
#' print(result)
#'
#' # Example with separate columns
#' data <- data.frame(
#'   lat_deg = c(34, 40),
#'   lat_min = c(30, 45),
#'   lat_sec = c(0, 0),
#'   lon_deg = c(118, 73),
#'   lon_min = c(15, 58),
#'   lon_sec = c(0, 0)
#' )
#' result <- dms_to_decimal(data, "lat_deg", "lon_deg", is_dms_symbol = FALSE)
#' print(result)

dms_to_decimal <- function(data,
                           lat_col,
                           lon_col,
                           is_dms_symbol = TRUE,
                           verbose = FALSE) {
  # Function to convert DMS string to decimal degrees
  dms_to_dd <- function(dms) {
    # Extract components using regex
    matches <- regmatches(dms, regexec("([0-9]+)°([0-9]+)'([0-9]+)\"([NSWE])", dms))

    # Convert degrees, minutes, seconds to decimal degrees
    degrees <- as.numeric(matches[[1]][2])
    minutes <- as.numeric(matches[[1]][3])
    seconds <- as.numeric(matches[[1]][4])
    direction <- matches[[1]][5]

    # Calculate decimal degrees
    decimal_deg <- degrees + minutes / 60 + seconds / 3600

    # Adjust sign based on direction
    if (direction %in% c("S", "W")) {
      decimal_deg <- -decimal_deg
    }

    return(decimal_deg)
  }

  if (is_dms_symbol) {
    if (verbose) message("Converting from DMS with symbols to decimal degrees.")

    # Apply the DMS to decimal conversion for latitude and longitude
    data$lat_dd <- sapply(data[[lat_col]], dms_to_dd)
    data$lon_dd <- sapply(data[[lon_col]], dms_to_dd)

  } else {
    if (verbose) message("Converting from separate degree, minute, second columns to decimal degrees.")

    # Convert latitude DMS to decimal degrees
    data$lat_dd <- with(data, {
      lat_dd <- abs(get(lat_col)) + get(paste0(lat_col, "_min")) / 60 + get(paste0(lat_col, "_sec")) / 3600

      # Adjust for southern hemisphere
      lat_dd <- ifelse(get(lat_col) < 0, -lat_dd, lat_dd)
      return(lat_dd)
    })

    # Convert longitude DMS to decimal degrees
    data$lon_dd <- with(data, {
      lon_dd <- abs(get(lon_col)) + get(paste0(lon_col, "_min")) / 60 + get(paste0(lon_col, "_sec")) / 3600

      # Adjust for western hemisphere
      lon_dd <- ifelse(get(lon_col) < 0, -lon_dd, lon_dd)
      return(lon_dd)
    })
  }

  if (verbose) {
    message("Conversion completed successfully.")
  }

  return(data)
}
