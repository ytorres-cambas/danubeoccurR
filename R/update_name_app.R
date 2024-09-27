#' Update Species Data in a Shiny MiniUI App
#'
#' This function launches a Shiny gadget within RStudio that allows users to manually update species data.
#' Users can select a species from a dropdown, enter a new value, and update the corresponding entry
#' in the data frame. Once done, the updated data frame is saved to the global environment and returned.
#'
#' @param species_data A data frame containing species information. It should have at least two columns:
#' `species` and `values`, where `species` contains species names, and `values` contains corresponding values (initially `NA`).
#'
#' @return Returns the updated species data frame with new values input by the user.
#' The updated data frame is also saved as `updated_species_data` in the global environment.
#'
#' @examples
#' \dontrun{
#' species_data <- data.frame(species = c("Species A", "Species B", "Species C"), values = c(NA, NA, NA))
#' updated_data <- update_name_app(species_data)
#' }
#'
#' @import shiny
#' @import miniUI
#' @export
update_name_app <- function(species_data) {
  # Define the miniUI
  mini_ui <- miniPage(
    gadgetTitleBar("Update Species Values"),
    miniContentPanel(
      selectInput("species_select", "Select Species:", choices = species_data$species),
      textInput("value_input", "Enter Value:"),
      actionButton("update_button", "Update Value"),
      actionButton("done", "Done"),  # Add Done button
      tableOutput("species_table")
    )
  )

  # Define server logic
  server <- function(input, output, session) {
    # Reactive value to hold the species data
    species_data_reactive <- reactiveVal(species_data)

    # Render the table
    output$species_table <- renderTable({
      species_data_reactive()
    })

    # Update value when the update button is clicked
    observeEvent(input$update_button, {
      current_data <- species_data_reactive()
      species_index <- which(current_data$species == input$species_select)

      # Update the selected value
      if (length(species_index) > 0) {
        current_data$values[species_index] <- input$value_input
        species_data_reactive(current_data)  # Update reactive value
      }
    })

    # Save the updated data frame and close the gadget
    observeEvent(input$done, {
      updated_data <- species_data_reactive()
      assign("updated_species_data", updated_data, envir = .GlobalEnv)  # Save to global environment
      stopApp(updated_data)  # Close the app and return updated data
    })
  }

  # Run the application
  runGadget(mini_ui, server)
}
