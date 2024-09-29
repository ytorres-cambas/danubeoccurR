#' Create Metadata File
#'
#' This function generates a metadata file describing a specified file stored in a folder.
#'
#' @param file_path The full path of the file for which metadata is to be created.
#' @param description A brief description of the file.
#' @param author The name of the author or creator of the file.
#' @param creation_date The date when the file was created (default is the current date).
#' @param notes Any additional notes about the file.
#' @param source A URL or reference for the source of the data.
#' @param metadata_folder Optional. The folder where the metadata file should be saved (default is the folder of the original file).
#' @return The path to the created metadata file.
#' @export
#'
#' @examples
#' metadata_file <- create_metadata_file("data/my_file.csv",
#'                                         description = "This file contains sample data.",
#'                                         author = "Jane Doe",
#'                                         source = "https://example.com/data-source",
#'                                         notes = "Data collected from various sources.")
create_metadata_file <- function(file_path,
                                 description,
                                 author,
                                 author_email = NULL,
                                 creation_date = Sys.Date(),
                                 notes = NULL,
                                 source = NULL,
                                 metadata_folder = dirname(file_path)) {

  # Check if the specified file exists
  if (!file.exists(file_path)) {
    stop("The specified file does not exist.")
  }

  # Create metadata content
  metadata_content <- paste(
    "File Name:", basename(file_path), "\n",
    "Description:", description, "\n",
    "Author:", author, "\n",
    "Author's email:", ifelse(is.null(author_email), "None", author_email), "\n",
    "Creation Date:", creation_date, "\n",
    "Source:", ifelse(is.null(source), "None", source), "\n",
    "Notes:", ifelse(is.null(notes), "None", notes), "\n"
  )

  # Define metadata file name
  metadata_file_name <- paste0(tools::file_path_sans_ext(basename(file_path)), "_metadata.txt")
  metadata_file_path <- file.path(metadata_folder, metadata_file_name)

  # Write metadata to file
  writeLines(metadata_content, metadata_file_path)

  # Return the path to the created metadata file
  print(paste0("Metadata file for ", basename(file_path),
               " created at ", metadata_file_path))
  return(metadata_file_path)
}
