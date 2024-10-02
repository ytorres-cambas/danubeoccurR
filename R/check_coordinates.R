#' Check Coordinates for WGS84 and Decimal Degrees and Convert to Numeric
#'
#' This function checks if the coordinates in a data frame are in the WGS84
#' format and in decimal degrees. It ensures that the latitude values are
#' between -90 and 90, and the longitude values are between -180 and 180.
#' If the `convert_to_numeric` parameter is `TRUE`, it attempts to convert
#' non-numeric coordinates to numeric using `as.numeric()`. It also prints
#' detailed information if `verbose` is set to `TRUE`, and returns the updated
#' data frame with numeric coordinates.
#'
#' @param df A data frame containing the coordinates.
#' @param lat_col The name of the column containing latitude values.
#' @param lon_col The name of the column containing longitude values.
#' @param convert_to_numeric Logical. If `TRUE`, attempts to convert the
#' coordinates to numeric. Default is `TRUE`.
#' @param verbose Logical. If `TRUE`, prints detailed information about invalid
#' and non-numeric coordinates. Default is `FALSE`.
#' @return A list containing:
#' \itemize{
#'   \item `df`: The updated data frame with latitude and longitude as numeric.
#'   \item `invalid_latitudes`: Indices of latitude values that are out of range or invalid.
#'   \item `invalid_longitudes`: Indices of longitude values that are out of range or invalid.
#'   \item `non_numeric_latitudes`: Indices of non-numeric latitude values (if any).
#'   \item `non_numeric_longitudes`: Indices of non-numeric longitude values (if any).
#'   \item `rows_changed`: Number of rows with changes after attempting conversion to numeric.
#' }
#' @examples
#' # A data frame with coordinates
#' df <- data.frame(latitude = c("45.5", "91", NA, "invalid", "12.5"),
#'                  longitude = c("-123.5", "200", "text", "-100", "55"))
#'
#' # A list with results
#' list_result <- check_coordinates(df,
#'  "latitude",
#'  "longitude",
#'  convert_to_numeric = TRUE,
#'  verbose = TRUE)
#'
#' @export
check_coordinates <- function(df, lat_col, lon_col, convert_to_numeric = TRUE, verbose = FALSE) {

  # Check if the input is a data frame
  if (!is.data.frame(df)) {
    stop("The 'df' parameter must be a data frame.")
  }

  # Check if latitude and longitude columns exist
  if (!lat_col %in% colnames(df)) {
    stop(paste("The latitude column", lat_col, "is not present in the data frame."))
  }

  if (!lon_col %in% colnames(df)) {
    stop(paste("The longitude column", lon_col, "is not present in the data frame."))
  }

  # Extract latitude and longitude columns
  latitudes <- df[[lat_col]]
  longitudes <- df[[lon_col]]

  # Initialize empty vectors to hold indices of invalid values
  invalid_latitudes <- numeric(0)
  invalid_longitudes <- numeric(0)
  non_numeric_latitudes <- numeric(0)
  non_numeric_longitudes <- numeric(0)

  # Track rows that change during conversion
  rows_changed <- 0

  # Optionally convert to numeric
  if (convert_to_numeric) {
    # Count rows before conversion where lat or lon is not numeric
    non_numeric_lat_before <- sum(is.na(suppressWarnings(as.numeric(latitudes))) & !is.na(latitudes))
    non_numeric_lon_before <- sum(is.na(suppressWarnings(as.numeric(longitudes))) & !is.na(longitudes))

    latitudes <- suppressWarnings(as.numeric(latitudes))
    longitudes <- suppressWarnings(as.numeric(longitudes))

    # Count rows after conversion where lat or lon is numeric
    non_numeric_lat_after <- sum(is.na(latitudes) & !is.na(df[[lat_col]]))
    non_numeric_lon_after <- sum(is.na(longitudes) & !is.na(df[[lon_col]]))

    # Calculate how many rows were successfully changed by as.numeric()
    rows_changed <- (non_numeric_lat_before - non_numeric_lat_after) +
      (non_numeric_lon_before - non_numeric_lon_after)
  }

  # Check for non-numeric values
  non_numeric_latitudes <- which(is.na(latitudes) & !is.na(df[[lat_col]]))
  non_numeric_longitudes <- which(is.na(longitudes) & !is.na(df[[lon_col]]))

  # Check for invalid latitude values (not between -90 and 90)
  invalid_latitudes <- which(latitudes < -90 | latitudes > 90)

  # Check for invalid longitude values (not between -180 and 180)
  invalid_longitudes <- which(longitudes < -180 | longitudes > 180)

  # Verbose output
  if (verbose) {
    message("### Coordinate Validation Summary ###")

    if (length(non_numeric_latitudes) > 0) {
      message("Non-numeric latitude values found at rows: ", paste(non_numeric_latitudes, collapse = ", "))
    } else {
      message("All latitude values are numeric.")
    }

    if (length(non_numeric_longitudes) > 0) {
      message("Non-numeric longitude values found at rows: ", paste(non_numeric_longitudes, collapse = ", "))
    } else {
      message("All longitude values are numeric.")
    }

    if (length(invalid_latitudes) > 0) {
      message("Invalid latitude values (out of -90 to 90 range) found at rows: ", paste(invalid_latitudes, collapse = ", "))
    } else {
      message("All latitude values are within the valid range (-90 to 90).")
    }

    if (length(invalid_longitudes) > 0) {
      message("Invalid longitude values (out of -180 to 180 range) found at rows: ", paste(invalid_longitudes, collapse = ", "))
    } else {
      message("All longitude values are within the valid range (-180 to 180).")
    }

    message("Rows with changes due to as.numeric conversion: ", rows_changed)
  }

  # Update the original data frame with numeric latitudes and longitudes
  df[[lat_col]] <- latitudes
  df[[lon_col]] <- longitudes

  # Return the updated data frame and summary results
  return(list(
    df = df,
    invalid_latitudes = invalid_latitudes,
    invalid_longitudes = invalid_longitudes,
    non_numeric_latitudes = non_numeric_latitudes,
    non_numeric_longitudes = non_numeric_longitudes,
    rows_changed = rows_changed
  ))
}
