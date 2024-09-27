#' Check Column Names of a Data Frame
#'
#' This function checks if the column names of a data frame correspond to the
#' names contained in a vector of standard column names. It returns the columns
#' that are missing in the data frame or columns in the data frame that are
#' not part of the standard names vector. If `verbose` is `TRUE`, the function
#' prints details about the missing and extra columns.
#'
#' @param df A data frame to be checked.
#' @param standard_names A character vector containing the standard column names.
#' @param verbose Logical. If `TRUE`, prints the missing and extra columns.
#' @return A list with two elements:
#' \itemize{
#'   \item `missing_columns`: Column names that are in `standard_names` but missing from the data frame.
#'   \item `extra_columns`: Column names that are in the data frame but not in `standard_names`.
#' }
#' @examples
#' df <- data.frame(a = 1:3, b = 4:6, d = 7:9)
#' standard_names <- c("a", "b", "c")
#' check_column_names(df, standard_names, verbose = TRUE)
#' @export
check_column_names <- function(df, standard_names, verbose = FALSE) {
  # Check if input is a data frame
  if (!is.data.frame(df)) {
    stop("The 'df' parameter must be a data frame.")
  }

  # Check if standard_names is a character vector
  if (!is.character(standard_names)) {
    stop("The 'standard_names' parameter must be a character vector.")
  }

  # Extract column names from the data frame
  df_colnames <- colnames(df)

  # Find missing columns (in standard_names but not in df_colnames)
  missing_columns <- setdiff(standard_names, df_colnames)

  # Find extra columns (in df_colnames but not in standard_names)
  extra_columns <- setdiff(df_colnames, standard_names)

  # If verbose is TRUE, print information about missing and extra columns
  if (verbose) {
    if (length(missing_columns) > 0) {
      cat("Missing columns in the data frame:\n")
      print(missing_columns)
    } else {
      cat("No missing columns.\n")
    }

    if (length(extra_columns) > 0) {
      cat("Extra columns in the data frame:\n")
      print(extra_columns)
    } else {
      cat("No extra columns.\n")
    }
  }

  # Return a list of missing and extra columns
  return(list(
    missing_columns = missing_columns,
    extra_columns = extra_columns
  ))
}

# Example usage:
# df <- data.frame(a = 1:3, b = 4:6, d = 7:9)
# standard_names <- c("a", "b", "c")
# check_column_names(df, standard_names, verbose = TRUE)
