#' Check for Duplicate Records in a Data Frame
#'
#' This function checks for duplicate records based on latitude, longitude,
#' sub-catchment ID, and species name. The date can be provided as either:
#' 1. A single column representing the year
#' 2. Three columns for day, month, and year
#' 3. A single column in the format dd/mm/yyyy.
#'
#' Duplicates are defined as multiple entries for the same species recorded at
#' the same location (same coordinates or sub-catchment ID) on the same date.
#'
#' @param data A data frame containing columns for latitude, longitude,
#' sub-catchment ID, and species name. Additionally, it can contain columns for
#' date in various formats.
#' @param lat_col The name of the column representing latitude.
#' @param lon_col The name of the column representing longitude.
#' @param subcatchment_col The name of the column representing sub-catchment ID.
#' @param species_col The name of the column representing species name.
#' @param year_col Optional. The name of the column representing the year (if date is year only).
#' @param day_col Optional. The name of the column representing the day (if date is split into day/month/year).
#' @param month_col Optional. The name of the column representing the month (if date is split into day/month/year).
#' @param date_col Optional. The name of the column representing a full date in the form dd/mm/yyyy.
#' @param delete_duplicates Logical. If `TRUE` (default), only one record is
#' kept for each group of duplicates. If `FALSE`, a new column is added to flag duplicates.
#' @param verbose Logical. If `TRUE`, details about duplicate records will be printed.
#' Default is `FALSE`.
#'
#' @return Returns a data frame with either duplicates removed or flagged.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   latitude = c(34.5, 34.5, 35.1, 35.1),
#'   longitude = c(-118.1, -118.1, -118.5, -118.5),
#'   subcatchment_id = c(101, 101, 102, 102),
#'   year = c(2021, 2021, 2021, 2021),
#'   species = c("Species A", "Species A", "Species B", "Species B")
#' )
#' result <- check_duplicates(data, "latitude", "longitude", "subcatchment_id", "species", year_col = "year", delete_duplicates = FALSE, verbose = TRUE)
#' print(result)

check_duplicates <- function(data, lat_col, lon_col, subcatchment_col, species_col,
                             year_col = NULL, day_col = NULL, month_col = NULL, date_col = NULL,
                             delete_duplicates = TRUE, verbose = FALSE) {

  # Check if necessary columns exist in the data
  required_cols <- c(lat_col, lon_col, subcatchment_col, species_col)
  if (!all(required_cols %in% colnames(data))) {
    stop("The data frame must contain the following columns: ", paste(required_cols, collapse = ", "))
  }

  # Process date columns
  if (!is.null(year_col)) {
    # If only the year is provided
    if (!(year_col %in% colnames(data))) {
      stop("The year column is not present in the data frame.")
    }
    data$full_date <- as.character(data[[year_col]])

  } else if (!is.null(day_col) & !is.null(month_col) & !is.null(year_col)) {
    # If day, month, and year are provided separately
    if (!(day_col %in% colnames(data)) | !(month_col %in% colnames(data)) | !(year_col %in% colnames(data))) {
      stop("The day, month, and/or year columns are not present in the data frame.")
    }
    data$full_date <- sprintf("%02d/%02d/%04d", data[[day_col]], data[[month_col]], data[[year_col]])

  } else if (!is.null(date_col)) {
    # If the full date is provided in dd/mm/yyyy format
    if (!(date_col %in% colnames(data))) {
      stop("The date column is not present in the data frame.")
    }
    data$full_date <- as.character(data[[date_col]])
  } else {
    stop("You must provide either a year column, or day, month, and year columns, or a date column.")
  }

  # Parse the full_date column into Date format
  data$full_date <- as.Date(data$full_date, format = "%d/%m/%Y")

  if (any(is.na(data$full_date))) {
    stop("Some date values could not be converted into proper Date objects.")
  }

  # Identify duplicates based on latitude/longitude and sub-catchment, date, and species
  data$duplicate_flag <- duplicated(data[, c(lat_col, lon_col, subcatchment_col, "full_date", species_col)]) |
    duplicated(data[, c(lat_col, lon_col, subcatchment_col, "full_date", species_col)], fromLast = TRUE)

  # If verbose is TRUE, print the number and details of duplicate records
  if (verbose) {
    num_duplicates <- sum(data$duplicate_flag)
    message("Number of duplicate records: ", num_duplicates)

    if (num_duplicates > 0) {
      message("Duplicate records:\n")
      print(data[data$duplicate_flag, ])
    }
  }

  if (delete_duplicates) {
    # If delete_duplicates is TRUE, remove duplicates (keeping the first occurrence)
    data <- data[!duplicated(data[, c(lat_col, lon_col, subcatchment_col, "full_date", species_col)]), ]

    if (verbose) {
      message("Duplicates removed, retaining the first occurrence.")
    }
  } else {
    # If delete_duplicates is FALSE, keep the data but flag duplicates
    if (verbose) {
      message("Duplicates flagged in the 'duplicate_flag' column.")
    }
  }

  # Remove helper column full_date before returning
  data$full_date <- NULL

  # Return the modified data frame
  return(data)
}
