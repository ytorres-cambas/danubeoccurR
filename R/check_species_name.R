#' Check Species Names with Variable Accuracy and Manual Editing Option
#'
#' This function checks the validity of species names in a data frame using the `check_names`
#' function from the `specleanr` package, allowing for flexible matching accuracy and synonym lookup.
#' It extends `check_names` by enabling loops through different matching accuracy levels if required.
#' Additionally, if `manual = TRUE`, the function launches a Shiny app to allow for manual editing of
#' the names after the automatic checking process is complete.
#'
#' @param data A data frame containing species names.
#' @param col_species_name A character string specifying the column in `data` with species names.
#' @param verbose Logical, if `TRUE`, additional information will be printed during execution.
#' @param target_accuracy Numeric, the starting accuracy (in percentage) for species name matching.
#' @param accuracy_decrement Numeric, optional. If provided, the function will loop through decreasing
#' accuracy levels, decrementing by this value.
#' @param synonym Logical, if `TRUE`, synonym names will be considered when checking species names.
#' @param manual Logical, if `TRUE`, after the name-checking process, the function will call a Shiny app
#' for manual editing of the results.
#'
#' @return If `accuracy_decrement` is `NULL`, the function returns a single data frame with checked species names.
#' If `accuracy_decrement` is provided, it returns a list of data frames for each accuracy level.
#' If `manual = TRUE`, the function will return a manually updated data frame after the Shiny app interaction.
#' @importFrom specleanr check_names
#' @importFrom dplyr filter select
#' @export
#'
#' @examples
#' \dontrun{
#'   library(specleanr)
#'   data <- data.frame(species = c("Salmo salar", "Oncorhynchus mykiss"))
#'   result <- check_species_name(data, col_species_name = "species", target_accuracy = 95)
#' }
check_species_name <- function(
    data,
    col_species_name,
    verbose = FALSE,
    target_accuracy = 90,
    accuracy_decrement = NULL,
    synonym = FALSE,
    manual = FALSE
) {
  # Check if the 'data' parameter is missing
  if (missing(data)) {
    stop("Error: A data frame with species names is required.")
  }

  # Check if 'data' is a data frame
  if (!is.data.frame(data)) {
    stop("Error: The 'data' parameter must be a data frame.")
  }

  # Check if the 'col_species_name' parameter is provided
  if (missing(col_species_name)) {
    stop("Error: The name of the column with species names is required.")
  }

  # Check if 'col_species_name' corresponds to a column in the 'data' data frame
  if (!col_species_name %in% colnames(data)) {
    stop(paste("Error: The column", col_species_name, "is not present in the provided data frame."))
  }

  if (is.null(accuracy_decrement)) {
    # No decrement, use target_accuracy to check names
    message(paste("Finding match at FishBase with", target_accuracy, "% accuracy"))

    # Call check_names from specleanr
    df_name_checked <- specleanr::check_names(
      data = data,
      colsp = col_species_name,
      verbose = verbose,
      pct = target_accuracy,
      merge = TRUE,
      sn = synonym
    )

    # Change NA with "No match found"
    df_name_checked$speciescheck <- ifelse(is.na(df_name_checked[["speciescheck"]]), "No match found",
           df_name_checked[["speciescheck"]])

  } else {
    # If accuracy_decrement is provided, loop through different accuracy levels
    message(paste("Finding match at FishBase with decreasing accuracy starting from",
                  target_accuracy, "%, using steps of", accuracy_decrement, "% for each decrement."))

    # Initialize list to store results
    accuracy_levels <- seq(target_accuracy, accuracy_decrement, by = -accuracy_decrement)
    df_name_checked <- vector("list", length(accuracy_levels))
    names(df_name_checked) <- accuracy_levels

    # Loop through different accuracy values
    for (a in accuracy_levels) {
      message(paste("Current target accuracy:", a))

      df_name_checked[[as.character(a)]] <- specleanr::check_names(
        data = data,
        colsp = col_species_name,
        verbose = verbose,
        pct = a,  # Decrementing accuracy level
        merge = TRUE,
        sn = synonym
      )
      # For the next loop use occurrences that did not match
      data <- df_name_checked[[as.character(a)]] |>
        dplyr::filter(is.na(speciescheck)) |>
        dplyr::select(-speciescheck)
    }
  }

  if(manual){
    # Call the Shiny app for manual editing using the last accuracy level's results
    if (is.null(accuracy_decrement)) {
      # If no accuracy_decrement, manually edit the single data frame
      df_name_checked_manual <- update_name_app(df_name_checked, col_species_name, "speciescheck")
    } else {
      # If accuracy_decrement is used, manually edit the last accuracy level's data frame
      df_name_checked_manual <- update_name_app(df_name_checked[[as.character(min(accuracy_levels))]], col_species_name, "speciescheck")
    }
    return(df_name_checked_manual)
  } else {
    return(df_name_checked)  # Returns a single data frame or a list of data frames based on accuracy_decrement
  }
}
