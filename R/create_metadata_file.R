#' Create Metadata File
#'
#' This function generates a metadata file describing a specified file stored in a folder.
#'
#' @param file_path Character. The full path of the file for which metadata is to be created.
#' @param description Character. A brief description of the file.
#' @param author Character. The name of the author or creator of the file.
#' @param author_email Character. The email of the author or creator of the file.
#' @param creation_date Date. The date when the file was created (default is the current date).
#' @param source Character. A URL or reference for the source of the data.
#' @param licence Character. The license under which the dataset is provided. Common licenses include:
#'   - `CC0 1.0 (Public Domain Dedication)` - No rights reserved, free for any use.
#'   - `CC BY 4.0 (Attribution)` - Free to use, with attribution.
#'   - `CC BY-SA 4.0 (Attribution-ShareAlike)` - Free to use, with attribution and share-alike.
#'   - `ODbL (Open Database License)` - Allows use, modification, and sharing, but derivatives must remain under ODbL.
#'   - `CC BY-NC 4.0 (Attribution-NonCommercial)` - Non-commercial use only, with attribution.
#'   - `CC BY-ND 4.0 (Attribution-NoDerivatives)` - You can share, but not modify the dataset.
#'   - `GNU GPL` - Allows reuse and modification, but derivatives must also be under GPL.
#'   - `MIT License` - Permissive, allows reuse in any context with attribution.
#'   Specify the license and provide a link if applicable (e.g., `CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/)`).
#' @param method Character. A short description of actions conducted to obtain or modify the dataset (e.g., "crop", "filter") or an indication of the script used to obtain or modify the dataset.
#' @param repo Character. A URL or reference to the repository where the script used to obtain or modify the dataset can be found (e.g., GitHub, Zenodo).
#' @param doi Character. The Digital Object Identifier (DOI) for the dataset or related publication, if available.
#' @param version Character. The version of the file. If not provided, it will be automatically assigned.
#' @param notes Character. Any additional notes about the file.
#' @param metadata_folder Character. The folder where the generated metadata file will be saved.
#' If not specified, the metadata file will be saved in the same folder as the file.
#'
#' @return A metadata file saved in the specified metadata folder or the same folder as the file if `metadata_folder` is not provided.
#'
#'
#' @examples
#' \dontrun{
#'    create_metadata_file(file_path = "data/my_data.csv",
#'                         description = "Sample dataset",
#'                         author = "John Doe",
#'                         author_email = "john.doe@example.com",
#'                         creation_date = Sys.Date(),
#'                         source = "https://example.com",
#'                         licence = "CC BY 4.0",
#'                         method = "Filtered using script XYZ"L,
#'                         repo = "https://github.com/repo",
#'                         doi = "10.1234/zenodo.1234567",
#'                         notes = "No additional notes")
#' }
#' @export

create_metadata_file <- function(file_path,
                                 description,
                                 author,
                                 author_email = NULL,
                                 creation_date = Sys.Date(),
                                 source = NULL,
                                 licence = NULL,
                                 method = NULL,
                                 repo = NULL,
                                 doi = NULL,
                                 notes = NULL,
                                 metadata_folder = dirname(file_path)) {

  # Check if the specified file exists
  if (!file.exists(file_path)) {
    stop("The specified file does not exist.")
  }

  # Define metadata file name
  metadata_file_name <- paste0(tools::file_path_sans_ext(basename(file_path)), "_metadata.txt")
  metadata_file_path <- file.path(metadata_folder, metadata_file_name)

  # Automatically assign file version

    if (file.exists(metadata_file_path)) {
      # Read the existing metadata file to check the last version
      metadata_lines <- readLines(metadata_file_path)
      version_line <- metadata_lines[grep("Version:", metadata_lines)]
      last_version <- sub("Version: ", "", version_line)
      version_parts <- as.numeric(unlist(strsplit(last_version, "\\.")))
      # Increment the version number
      version <- paste0(version_parts[1], ".", version_parts[2] + 1)
    } else {
      # Start with version 1.0 if no metadata file exists
      version <- "1.0"
    }


  # Create metadata content
  metadata_content <- paste(
    "File Name:", basename(file_path), "\n",
    "Description:", description, "\n",
    "Author:", author, "\n",
    "Author's email:", ifelse(is.null(author_email), "None", author_email), "\n",
    "Creation Date:", creation_date, "\n",
    "Source:", ifelse(is.null(source), "None", source), "\n",
    "Licence:", ifelse(is.null(licence), "None", licence), "\n",
    "Method:", ifelse(is.null(method), "None", method), "\n",
    "Repository:", ifelse(is.null(repo), "None", repo), "\n",
    "DOI:", ifelse(is.null(doi), "None", doi), "\n",
    "Version:", version, "\n",
    "Notes:", ifelse(is.null(notes), "None", notes), "\n"
  )

  # Write metadata to file
  writeLines(metadata_content, metadata_file_path)

  # Return the path to the created metadata file
  print(paste0("Metadata file for ", basename(file_path),
               " created at ", metadata_file_path))
  return(metadata_file_path)
}
