#' Clean GBIF Occurrence Records
#'
#' This function cleans species occurrence records downloaded using the
#' `download_gbif_records()` function from this package. It applies filters based on
#' coordinate precision, uncertainty, and geographic buffers, removes records with low precision
#' or uncertainty, excludes records near country, capital, and zoo/herbaria centroids using methods
#' from the `CoordinateCleaner` package, and eliminates duplicates based on longitude,
#' latitude, speciesKey, and datasetKey. Additionally, it removes records
#' that do not contain species or year information.
#'
#' The function uses the following methods from the `CoordinateCleaner` package:
#' - `cc_cen()`: Removes records near country centroids.
#' - `cc_cap()`: Removes records near capital centroids.
#' - `cc_inst()`: Removes records near zoo or herbaria centroids.
#'
#' @param df A data frame containing species occurrence records,
#'   typically downloaded using the `download_gbif_records()` function from this package.
#' @param coordinatePrecision Numeric. Remove records below this precision.
#' @param coordinateUncertaintyInMeters Numeric. Remove records below this uncertainty or specific common values (301, 3036, 999, 9999).
#' @param buffer_centroid_countries Numeric. Buffer size for removing records near country centroids.
#' @param buffer_centroid_capitals Numeric. Buffer size for removing records near capital centroids.
#' @param buffer_centroid_zoo_herbaria Numeric. Buffer size for removing records near zoo or herbaria centroids.
#' @param decimalLongitude String. Column name for longitude. Default is "decimalLongitude".
#' @param decimalLatitude String. Column name for latitude. Default is "decimalLatitude".
#' @param speciesKey String. Column name for species key. Default is "speciesKey".
#' @param datasetKey String. Column name for dataset key. Default is "datasetKey".
#' @param year String. Column name for the year of occurrence. Default is "year".
#' @param species String. Column name for the species. Default is "species".
#' @param duplicate Logical. Default is FALSE. Whether or not to remove duplicates that have
#' the same longitude, latitude, speciesKey, and datasetKey.
#'
#' @return A cleaned data frame with species occurrences.
#' @import CoordinateCleaner
#' @importFrom dplyr %>%
#' @examples
#' # Example usage
#' df <- download_gbif_records(...)
#' cleaned_df <- clean_gbif(df, coordinatePrecision = 0.01,
#'                          coordinateUncertaintyInMeters = 100,
#'                          buffer_centroid_countries = 10,
#'                          buffer_centroid_capitals = 10,
#'                          buffer_centroid_zoo_herbaria = 10,
#'                          decimalLongitude = "decimalLongitude",
#'                          decimalLatitude = "decimalLatitude",
#'                          speciesKey = "speciesKey",
#'                          datasetKey = "datasetKey",
#'                          year = "year",
#'                          species = "species")
#'
#' @export

clean_gbif <- function(df,
                       coordinatePrecision = NULL,
                       coordinateUncertaintyInMeters = NULL,
                       buffer_centroid_countries = NULL,
                       buffer_centroid_capitals = NULL,
                       buffer_centroid_zoo_herbaria = NULL,
                       decimalLongitude = "decimalLongitude",
                       decimalLatitude = "decimalLatitude",
                       speciesKey = "speciesKey",
                       datasetKey = "datasetKey",
                       year = "year",
                       species = "species",
                       duplicate = FALSE) {

  # Ensure required columns are present in the data frame
  required_columns <- c(decimalLongitude, decimalLatitude, speciesKey, datasetKey, year, species)

  if (!all(required_columns %in% colnames(df))) {
    stop("Missing required columns: ", paste(setdiff(required_columns, colnames(df)), collapse = ", "))
  }

  # Initial number of records
  before_filter <- nrow(df)

  # Remove records based on coordinatePrecision
  if (!is.null(coordinatePrecision)) {
    df <- df %>%
      dplyr::filter(is.na(!!rlang::sym("coordinatePrecision")) |
                      !!rlang::sym("coordinatePrecision") > coordinatePrecision)
  }

  # Remove records based on coordinateUncertaintyInMeters
  if (!is.null(coordinateUncertaintyInMeters)) {
    df <- df %>%
      dplyr::filter(is.na(!!rlang::sym("coordinateUncertaintyInMeters")) |
                      !!rlang::sym("coordinateUncertaintyInMeters") <= coordinateUncertaintyInMeters |
                      ! (!!rlang::sym("coordinateUncertaintyInMeters") %in% c(301, 3036, 999, 9999)))
  }

  # Remove duplicates based on longitude, latitude, speciesKey, and datasetKey
  if(duplicate){
    before_duplicates <- nrow(df)
  df <- df %>%
    dplyr::distinct(!!rlang::sym(decimalLongitude), !!rlang::sym(decimalLatitude), !!rlang::sym(speciesKey), !!rlang::sym(datasetKey), .keep_all = TRUE)
  after_duplicates <- nrow(df)
  removed_duplicates <- before_duplicates - after_duplicates
  message("Removed ", removed_duplicates, " duplicate records.")
  }


  # Remove records near country centroids (using CoordinateCleaner::cc_cen)
  if (!is.null(buffer_centroid_countries)) {
    before_countries <- nrow(df)
    df <- suppressMessages(CoordinateCleaner::cc_cen(df, buffer = buffer_centroid_countries, value = "clean"))
    after_countries <- nrow(df)
    removed_countries <- before_countries - after_countries
    message("Testing country centroids: Removed ", removed_countries, " records.")
  }

  # Remove records near capital centroids (using CoordinateCleaner::cc_cap)
  if (!is.null(buffer_centroid_capitals)) {
    before_capitals <- nrow(df)
    df <- suppressMessages(CoordinateCleaner::cc_cap(df, buffer = buffer_centroid_capitals, value = "clean"))
    after_capitals <- nrow(df)
    removed_capitals <- before_capitals - after_capitals
    message("Testing country capitals: Removed ", removed_capitals, " records.")
  }

  # Remove records near zoos and herbaria (using CoordinateCleaner::cc_inst)
  if (!is.null(buffer_centroid_zoo_herbaria)) {
    before_zoo <- nrow(df)
    df <- suppressMessages(CoordinateCleaner::cc_inst(df, buffer = buffer_centroid_zoo_herbaria, value = "clean"))
    after_zoo <- nrow(df)
    removed_zoo <- before_zoo - after_zoo
    message("Testing biodiversity institutions: Removed ", removed_zoo, " records.")
  }

  # Remove records with missing year or species
  before_year <- nrow(df)
  df <- df %>%
    dplyr::filter(!is.na(!!rlang::sym(year))) %>%
    dplyr::filter(!is.na(!!rlang::sym(species))) %>%
    dplyr::filter(!!rlang::sym(species) != "")
  after_year <- nrow(df)
  removed_year <- before_year - after_year
  message("Testing missing year or species: Removed ", removed_year, " records.")

  message("Retained ", nrow(df), " out of ", before_filter, " records after cleaning.")

  return(df)
}


