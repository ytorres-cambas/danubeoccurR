#' Automatically import CSV or Excel files and convert character columns to UTF-8
#'
#' This function imports data from either a CSV or Excel file, detects the file encoding,
#' and converts character columns to UTF-8. It supports different delimiters for CSV files
#' and allows specifying a sheet for Excel files. Additionally, the function can handle
#' header detection and custom separators for CSV files.
#'
#' @param file_path A string specifying the path to the file to import.
#' @param file_type A string specifying the file type. Options are `"csv"` or `"xlsx"`.
#'   Default is `"csv"`.
#' @param sheet A string or integer specifying the sheet to read from an Excel file.
#'   Ignored if `file_type` is `"csv"`. Default is `NULL`, which means the first sheet is read.
#' @param header A logical value indicating whether the first row contains column names.
#'   Default is `TRUE`.
#' @param sep A string specifying the separator used to separate columns in a CSV file.
#'   Ignored if `file_type` is `"xlsx"`. Default is `","`.
#'
#' @return A data frame with all character columns converted to UTF-8 encoding.
#'
#' @details
#' - For CSV files, the function automatically detects the file's encoding using the
#'   `stringi::stri_enc_detect()` function and reads the data with `data.table::fread()`.
#' - For Excel files, the function uses `readxl::read_excel()` to read the data.
#' - After importing, all character columns are explicitly converted to UTF-8 encoding to ensure consistency.
#' - You can specify whether the first row contains column names (`header`) and adjust
#'   the separator (`sep`) for CSV files.
#' - The `sheet` parameter allows you to specify which sheet to read when importing Excel files.
#'
#' @examples
#' \dontrun{
#' # Import a CSV file with UTF-8 encoding and custom separator
#' df_csv <- auto_import_utf8("data/file.csv", file_type = "csv", header = TRUE, sep = ";")
#'
#' # Import an Excel file, reading from the first sheet by default
#' df_xlsx <- auto_import_utf8("data/file.xlsx", file_type = "xlsx")
#'
#' # Import an Excel file, specifying a sheet and no header
#' df_xlsx_sheet <- auto_import_utf8("data/file.xlsx", file_type = "xlsx", sheet = "Sheet2", header = FALSE)
#' }
#'
#' @import data.table
#' @import readxl
#' @importFrom stringi stri_enc_detect
#' @export
auto_import_utf8 <- function(file_path,
                             file_type = c("csv", "xlsx"),
                             sheet = NULL,
                             header = TRUE,
                             sep = ",") {

  # Match the file type argument
  file_type <- match.arg(file_type)

  if (file_type == "csv") {
    # Detect encoding for CSV files
    encoding <- stri_enc_detect(file_path)[[1]]$Encoding[1]

    # Read CSV file with detected encoding
    df <- fread(file_path, encoding = encoding, header = header, sep = sep)

    # Convert character columns to UTF-8
    df[] <- lapply(df, function(col) {
      if (is.character(col)) {
        iconv(col, from = encoding, to = "UTF-8")
      } else {
        col  # Leave non-character columns unchanged
      }
    })

  } else if (file_type == "xlsx") {
    # Read Excel file
    if (is.null(sheet)) {
      # If no sheet is specified, read the first sheet
      df <- read_excel(file_path, col_names = header)
    } else {
      # Read the specified sheet
      df <- read_excel(file_path, sheet = sheet, col_names = header)
    }

    # Explicitly convert character columns to UTF-8 (Excel files should already be UTF-8, but this ensures consistency)
    df[] <- lapply(df, function(col) {
      if (is.character(col)) {
        iconv(col, from = "UTF-8", to = "UTF-8")  # Ensure it's UTF-8
      } else {
        col  # Leave non-character columns unchanged
      }
    })
  }

  return(df)
}
