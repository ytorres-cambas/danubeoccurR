#' Run the Metadata Generator Mini App (Dublin Core)
#'
#' This function launches a miniUI Shiny app that allows the user to fill in metadata
#' according to Dublin Core standards and download an XML file.
#'
#' @return Launches a miniUI Shiny app.
#' @export
run_metadata_app <- function() {
  library(shiny)
  library(miniUI)
  library(xml2)

  # Define UI using miniPage and miniUI
  ui <- miniPage(
    gadgetTitleBar("Metadata Generator (Dublin Core)"),
    miniContentPanel(
      textInput("title", "Title", "Fish Species Occurrences in the Danube River Basin"),
      textInput("creator", "Creator", "Your Name or Organization"),
      textInput("subject", "Subject", "Fish Species, Danube River Basin, Occurrences"),
      textAreaInput("description", "Description", "This dataset contains fish species occurrence records for the Danube River Basin collected from various sources."),
      textInput("publisher", "Publisher", "Your Organization"),
      textInput("contributor", "Contributor", "Collaborators or Project Partners"),
      dateInput("date", "Date", Sys.Date()),
      textInput("type", "Type", "Dataset"),
      textInput("format", "Format", "text/xml"),
      textInput("identifier", "Identifier (DOI)", "https://doi.org/xx.xxxx/xxxxxx"),
      textInput("source", "Source", "GBIF, Danube4All Project"),
      textInput("language", "Language", "en"),
      textInput("rights", "Rights", "CC BY 4.0"),
      textInput("coverage", "Coverage", "Danube River Basin")
    ),
    miniButtonBlock(
      downloadButton("downloadXML", "Download XML")
    )
  )

  # Define server logic to generate the XML file
  server <- function(input, output, session) {

    # Function to create the XML metadata
    generate_metadata <- function() {
      doc <- xml_new_root("metadata", ns = c(dc = "http://purl.org/dc/elements/1.1/"))
      xml_add_child(doc, "dc:title", input$title)
      xml_add_child(doc, "dc:creator", input$creator)
      xml_add_child(doc, "dc:subject", input$subject)
      xml_add_child(doc, "dc:description", input$description)
      xml_add_child(doc, "dc:publisher", input$publisher)
      xml_add_child(doc, "dc:contributor", input$contributor)
      xml_add_child(doc, "dc:date", input$date)
      xml_add_child(doc, "dc:type", input$type)
      xml_add_child(doc, "dc:format", input$format)
      xml_add_child(doc, "dc:identifier", input$identifier)
      xml_add_child(doc, "dc:source", input$source)
      xml_add_child(doc, "dc:language", input$language)
      xml_add_child(doc, "dc:rights", input$rights)
      xml_add_child(doc, "dc:coverage", input$coverage)

      return(doc)
    }

    # Download handler for the XML file
    output$downloadXML <- downloadHandler(
      filename = function() {
        paste("metadata", Sys.Date(), ".xml", sep = "")
      },
      content = function(file) {
        doc <- generate_metadata()
        write_xml(doc, file)
      }
    )

    # Close the gadget when done
    observeEvent(input$done, {
      stopApp()
    })
  }

  # Run the gadget with miniUI
  shinyApp(ui = ui, server = server)
}
