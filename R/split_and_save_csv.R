#' Split a data frame based on the values of a column and save each subset as a CSV file
#'
#' This function splits a data frame by the unique values in a specified column and
#' saves each subset as a CSV file. The CSV files will be named based on the unique
#' values in the specified column.
#'
#' @param df A data frame to be split.
#' @param split_col The name of the column to split the data frame by (as a string).
#' @param output_dir The directory where the CSV files will be saved. Default is the current working directory.
#' @param sep The separator used in the CSV files. Default is a comma (`,`).
#'
#' @return No return value. CSV files are saved to disk.
#'
#' @examples
#' \dontrun{
#' # Assuming df has a column named "datasetName"
#' split_and_save_csv(df, split_col = "datasetName", output_dir = "output/")
#' }
#'
#' @importFrom data.table fwrite
#' @export
split_and_save_csv <- function(df, split_col, output_dir = ".", sep = ",") {

  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Split the data frame by the unique values in the split_col
  split_data <- split(df, df[[split_col]])

  # Iterate over each subset and save it as a CSV file
  lapply(names(split_data), function(dataset_name) {
    # Define the file name and path
    file_name <- paste0(dataset_name, ".csv")
    file_path <- file.path(output_dir, file_name)

    # Save the subset to a CSV file
    fwrite(split_data[[dataset_name]], file = file_path, sep = sep)
  })

  message("CSV files have been saved to ", normalizePath(output_dir))
}
