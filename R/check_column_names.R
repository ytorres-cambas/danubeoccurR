#' Check Data Frame Column Names Against Darwin Core Standard Terms
#'
#' This function checks that the column names of a data frame (or multiple data frames)
#' are part of a set of standard terms from the Darwin Core. It will output the columns
#' that do not follow the standard names and can merge data frames if specified.
#'
#' @param df_input A data frame or a list of data frames to check.
#' @param standard_terms A character vector containing the standard column names (default is `dwc_names`).
#' @param verbose A logical value indicating whether to output the columns that do not match the standard terms (default is TRUE).
#' @param merge A logical value indicating whether to merge data frames if all have the same names (default is FALSE).
#'
#' @return If merging is enabled and all data frames have the same column names, it returns a merged data frame.
#'         If verbosity is enabled, it prints out any columns that do not match the standard terms.
#' @examples
#' # Example usage with a single data frame
#' check_column_names(fish_data)
#'
#' # Example usage with a list of data frames and same column names
#' check_column_names((list(fish_data, fish_data),merge = T))
#'
#' # Example usage with a list of data frames and different column names
#' fish_data1 <- fish_data
#' fish_data2 <- fish_data %>%
#' rename(latitude = decimalLatitude, longitude = decimalLongitude)
#' check_column_names(list(fish_data1, fish_data2), merge = T)
#'
#' @export
check_column_names <- function(df_input,
                               standard_terms = dwc_names,
                               verbose = TRUE,
                               merge = FALSE) {

  # Ensure df_input is a list of data frames, even if a single data frame is provided
  if (is.data.frame(df_input)) {
    df_input <- list(df_input)
  }

  # Initialize a list to store non-standard columns for each data frame
  non_standard_results <- list()

  # Loop through each data frame in the list
  for (i in seq_along(df_input)) {
    df <- df_input[[i]]
    df_name <- paste0("DataFrame_", i)

    # Identify non-standard columns
    non_standard_cols <- setdiff(colnames(df), standard_terms)

    if (verbose) {
      if (length(non_standard_cols) > 0) {
        message(paste("Columns in", df_name, "that do not follow Darwin Core standard terms:"))
        message(paste(non_standard_cols, collapse = ", "))
      } else {
        message(paste("All columns in", df_name, "follow suggested standard terms."))
      }
    }
  }

  # Check if merging is required
  if (merge) {
    # Get the column names from each data frame
    col_names_list <- lapply(df_input, colnames)

    # Check if all data frames have the same column names
    if (length(unique(col_names_list)) == 1) {
      merged_df <- do.call(rbind, df_input)
      message("Data frames have been merged successfully.")
      return(merged_df)
    } else {
      # If column names differ, report unique columns for each data frame
      message("Data frames have different column names. Merging is not possible.
              Unique columns for each data frame are:")

      for (i in seq_along(df_input)) {
        df <- df_input[[i]]
        df_name <- paste0("DataFrame_", i)
        unique_cols <- setdiff(colnames(df), Reduce(intersect, col_names_list))

        if (length(unique_cols) > 0) {
          message(paste("Unique columns in", df_name, ":"))
          message(paste(unique_cols, collapse = ", "))
        }
      }
    }
  }
}
