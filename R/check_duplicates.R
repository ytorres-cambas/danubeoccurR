#' Check for Duplicate Records in a Data Frame
#'
#' This function checks for duplicate records based on latitude, longitude,
#' sub-catchment ID, and species name. The date can be provided as a single column
#' representing the year.
#'
#' Duplicates are defined as multiple entries for the same species recorded at
#' the same location (same coordinates or sub-catchment ID) in the same year.
#'
#' @param data A data frame containing columns for latitude, longitude,
#' sub-catchment ID, and species name. Additionally, it must contain a column for
#' the year.
#' @param lat_col The name of the column representing latitude.
#' @param lon_col The name of the column representing longitude.
#' @param subcatchment_col The name of the column representing sub-catchment ID.
#' @param species_col The name of the column representing species name.
#' @param year_col The name of the column representing the year (mandatory).
#' @param delete_duplicates Logical. If `TRUE` (default), only one record is
#' kept for each group of duplicates. If `FALSE`, a new column is added to flag duplicates.
#' @param verbose Logical. If `TRUE`, details about duplicate records will be printed.
#' Default is `FALSE`.
#'
#' @return Returns a data frame with either duplicates removed or flagged,
#'         along with a summary of the number of rows with changes.
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
#' result <- check_duplicates(data,
#'                            "latitude",
#'                            "longitude",
#'                            "subcatchment_id",
#'                            "species",
#'                            year_col = "year",
#'                            delete_duplicates = FALSE,
#'                            verbose = TRUE)
#' print(result)

check_duplicates <- function(data,
                             lat_col,
                             lon_col,
                             subcatchment_col,
                             species_col,
                             year_col,
                             delete_duplicates = TRUE,
                             verbose = FALSE) {

  # Check if necessary columns exist in the data
  required_cols <- c(lat_col, lon_col, subcatchment_col, species_col, year_col)
  if (!all(required_cols %in% colnames(data))) {
    stop("The data frame must contain the following columns: ", paste(required_cols, collapse = ", "))
  }

  # Process year column
  if (!(year_col %in% colnames(data))) {
    stop("The year column is not present in the data frame.")
  }

  # Create a full_date column based on the year
  data$full_date <- as.character(data[[year_col]])

  # Identify duplicates based on latitude/longitude and sub-catchment, year, and species
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

  # Return the modified data frame along with the summary
  summary_changes <- sum(data$duplicate_flag)
  message("Number of rows with changes: ", summary_changes)

  return(data)
}

