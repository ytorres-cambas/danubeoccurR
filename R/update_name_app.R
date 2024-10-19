#' Update Species Data in a Shiny MiniUI App
#'
#' This function launches a Shiny gadget within RStudio that allows users to
#' manually update species data in a specified data frame. Only rows where the old species
#' name differs from the new species name (or where the new species name is NA) are shown for
#' selection and modification. Selecting a species from the dropdown filters the table to only
#' show the selected row for updating. Once completed, the entire original data frame is
#' returned with updated values in the specified column and an additional flag column.
#'
#' @param data A data frame containing species names and potentially other data.
#' @param old_names_col The column name in the data frame corresponding to the old species names.
#' @param new_names_col The column name in the data frame corresponding to the new species names to be updated.
#'
#' @return Returns the original data frame with the updated values in the new names column
#' and a flag indicating whether each row was manually updated.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(old_species = c("Chondostroma nasus", "Hucho ucho", "Anguila anguilla"),
#'                  new_species = c(NA, "Hucho ucho", NA), other_data = c(1, 2, 3))
#' updated_data <- update_name_app(df, "old_species", "new_species")
#' }
#'
#' @import shiny
#' @importFrom miniUI miniPage
#' @import dplyr
#' @export
update_name_app <- function(data, old_names_col, new_names_col) {
  # Ensure columns exist in the data frame
  if (!(old_names_col %in% colnames(data)) || !(new_names_col %in% colnames(data))) {
    stop("The specified columns do not exist in the data frame.")
  }

  # Filter rows where old names and new names differ, or where new names are NA
  filtered_data <- data[is.na(data[[new_names_col]]) | data[[old_names_col]] != data[[new_names_col]], ]

  # Define the miniUI
  mini_ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Update Species Values"),
    miniUI::miniContentPanel(
      shiny::selectInput("species_select", paste("Select", old_names_col, ":"),
                         choices = unique(filtered_data[[old_names_col]])),
      shiny::textInput("value_input", "Enter New Name:"),
      shiny::actionButton("update_button", "Update Value"),
      shiny::actionButton("done", "Done"),  # Add Done button
      # Wrap the table output in a div with scrollable style
      shiny::div(
        style = "height: 200px; overflow-y: auto;",  # Set height and enable vertical scroll
        shiny::tableOutput("species_table")
      )
    )
  )

  # Define server logic
  server <- function(input, output, session) {
    # Reactive value to hold the full species data (entire original data frame)
    species_data <- shiny::reactiveVal(data)

    # Render the table to show the row for the selected species without duplicates
    output$species_table <- shiny::renderTable({
      current_data <- species_data()

      # Filter to show only unique values in the old names column
      #unique_old_names <- unique(current_data[[old_names_col]])
      selected_species <- input$species_select

      # Filter the current data to show only the selected row
      selected_row <- current_data[current_data[[old_names_col]] == selected_species, ]
      #selected_row <- selected_row[selected_row[[old_names_col]] %in% unique_old_names, ]

      selected_row[, c(old_names_col, new_names_col)]  # Show only old and new names
    })

    # Store the original new names before any updates
    original_data <- shiny::reactiveVal(data)

    # Update value when the update button is clicked
    shiny::observeEvent(input$update_button, {
      current_data <- species_data()
      species_index <- which(current_data[[old_names_col]] == input$species_select)

      # Update the selected value in the new names column
      if (length(species_index) > 0) {
        current_data[[new_names_col]][species_index] <- input$value_input
        species_data(current_data)  # Update reactive value

        # Clear the text input after updating
        shiny::updateTextInput(session, "value_input", value = "")
      }
    })

    # Save the updated data frame and close the gadget
    shiny::observeEvent(input$done, {
      updated_data <- species_data()

      # Create the updated flag column by comparing original new_names with updated new_names
      updated_flag <- ifelse(original_data()[[new_names_col]] != updated_data[[new_names_col]], TRUE, FALSE)

      # Add the 'updated' flag column
      updated_data <- updated_data %>%
        mutate(manually_updated = updated_flag)

      shiny::stopApp(updated_data)  # Close the app and return updated data
    })
  }

  # Run the application
  shiny::runGadget(mini_ui, server)
}
