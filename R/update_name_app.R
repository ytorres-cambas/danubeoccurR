#' Update Species Data in a Shiny MiniUI App
#'
#' This function launches a Shiny gadget within RStudio that allows users to
#' manually update species data. Users can select a species from a dropdown,
#' enter a new name, and update the corresponding entry. Once completed, the
#' updated names are saved and returned as a data frame with one column for
#' the old names and another for the new names.
#'
#' @param species_name A string containing species names.
#'
#' @return Returns a data frame with one column for the old names and another
#' for the new names.
#'
#' @examples
#' \dontrun{
#' species_name <-  c("Species A", "Species B", "Species C")
#' updated_data <- update_name_app(species_name)
#' }
#'
#' @import shiny
#' @importFrom miniUI miniPage
#' @export
update_name_app <- function(species_name) {
  # Define the miniUI
  mini_ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Update Species Values"),
    miniUI::miniContentPanel(
      shiny::selectInput("species_select", "Select Species:",
                  choices = unique(species_name)),
      shiny::textInput("value_input", "Enter Value:"),
      shiny::actionButton("update_button", "Update Value"),
      shiny::actionButton("done", "Done"),  # Add Done button
      shiny::tableOutput("species_table")
    )
  )

  # Define server logic
  server <- function(input, output, session) {
    # Reactive value to hold the species data
    df_species_name <- data.frame("old_names" = unique(species_name),
                                   "new_names" = rep("NA",
                                                     times = length(species_name)))
    species_name_reactive <- shiny::reactiveVal(df_species_name)

    # Render the table
    output$species_table <- shiny::renderTable({
      species_name_reactive()
    })

    # Update value when the update button is clicked
    shiny::observeEvent(input$update_button, {
      current_data <- species_name_reactive()
      species_index <- which(current_data$old_names == input$species_select)

      # Update the selected value
      if (length(species_index) > 0) {
        current_data$new_names[species_index] <- input$value_input
        species_name_reactive(current_data)  # Update reactive value
      }
    })

    # Save the updated data frame and close the gadget
    shiny::observeEvent(input$done, {
      updated_data <- species_name_reactive()
      shiny::stopApp(updated_data)  # Close the app and return updated data
    })
  }

  # Run the application
  shiny::runGadget(mini_ui, server)
}
