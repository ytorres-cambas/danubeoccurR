#' Check Missing Values and Validity of Year Column
#'
#' This function checks for missing values in a specified column of a data frame
#' that contains years. It also ensures that the values are numeric and fall within
#' a valid range. If `transform_numeric` is `TRUE`, the function attempts to convert
#' non-numeric values to numeric.
#'
#' @param df A data frame containing the data to be checked.
#' @param col_year The name of the column in the data frame that contains the year values.
#' @param year_range A numeric vector of length 2 specifying the valid range of years
#' (e.g., `c(1900, 2024)`).
#' @param transform_numeric Logical. If `TRUE`, attempts to convert non-numeric year
#' values to numeric. Defaults to `TRUE`.
#' @return A list containing:
#' \itemize{
#'   \item `missing_values`: Indices of missing values in the year column.
#'   \item `invalid_years`: Indices of values that fall outside the valid year range.
#'   \item `updated_df`: A data frame with the updated year values and a flag indicating
#' whether the values were transformed.
#' }
#' @examples
#' df <- data.frame(year = c("2001", "2005", NA, "two thousand and ten", "2018", "2050"))
#' result <- check_year_column(df, "year", year_range = c(1800, 2024))
#' @export
check_year_column <- function(df, col_year, year_range = c(1800, 2024), transform_numeric = TRUE) {
  # Check if input is a data frame
  if (!is.data.frame(df)) {
    stop("The 'df' parameter must be a data frame.")
  }

  # Check if the column exists in the data frame
  if (!col_year %in% colnames(df)) {
    stop(paste("The column", col_year, "is not present in the data frame."))
  }

  # Extract the year column
  year_col <- df[[col_year]]

  # Check for missing values
  missing_values <- which(is.na(year_col))

  # Initialize a flag for transformed values
  transformed_flag <- rep(FALSE, length(year_col))

  # Check if transformation to numeric is requested
  if (transform_numeric) {
    # Attempt to convert to numeric
    transformed_year_col <- suppressWarnings(as.numeric(year_col))

    # Set the flag to TRUE where transformation occurred
    transformed_flag[is.na(year_col) & !is.na(transformed_year_col)] <- TRUE

    # Replace the original year column with the transformed values
    year_col[is.na(year_col)] <- transformed_year_col[is.na(year_col)]
  }

  # Check if the column is numeric after conversion (or initially)
  if (!is.numeric(year_col)) {
    stop("The column could not be converted to numeric.")
  }

  # Check for values outside the specified year range
  invalid_years <- which(year_col < year_range[1] | year_col > year_range[2])

  # Create an updated data frame
  updated_df <- df
  updated_df[[col_year]] <- year_col
  updated_df$transformed_flag <- transformed_flag

  # Return the results
  return(list(
    missing_values = missing_values,
    invalid_years = invalid_years,
    updated_df = updated_df
  ))
}
