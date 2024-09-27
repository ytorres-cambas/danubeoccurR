#' Interactive Point Snapping on Map
#'
#' This function allows users to interactively snap points on a map by clicking
#' and dragging markers. It returns a data frame with old and new coordinates,
#' and indicates whether the coordinates were updated.
#'
#' @param coords_df A data frame containing columns `lat` and `lng` for latitude and longitude.
#' @return A data frame with old and new coordinates and update status.
#' @importFrom shiny fluidPage leafletOutput renderLeaflet observeEvent reactiveVal shinyApp verbatimTextOutput
#' @importFrom leaflet addProviderTiles addMarkers leafletProxy options markerOptions
#' @export
#'
#' @examples
#' coords <- data.frame(id = 1:2, lat = c(37.7749, 34.0522), lng = c(-122.4194, -118.2437))
#' updated_coords <- snap_points_on_map(coords)

snap_points_on_map <- function(coords_df) {

  # Ensure input data frame has required columns
  if (!all(c("lat", "lng") %in% names(coords_df))) {
    stop("Input data frame must contain 'lat' and 'lng' columns.")
  }

  # Add an ID column if not present
  if (!"id" %in% names(coords_df)) {
    coords_df$id <- seq_len(nrow(coords_df))
  }

  # Define UI
  ui <- shiny::fluidPage(
    titlePanel("Drag Points on a Map"),
    leaflet::leafletOutput("map", width = "100%", height = "600px"),
    shiny::verbatimTextOutput("coordinates")
  )

  # Define server logic
  server <- function(input, output, session) {

    # Reactive dataframe to store points
    points <- shiny::reactiveVal(coords_df)

    # Render leaflet map with draggable markers
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(providers$CartoDB.Positron) %>%
        leaflet::addMarkers(data = points(),
                            lat = ~lat, lng = ~lng,
                            popup = ~paste("Lat:", lat, "<br>Lng:", lng),
                            layerId = ~id,
                            options = leaflet::markerOptions(draggable = TRUE))  # Enable dragging
    })

    # Update coordinates after dragging
    shiny::observeEvent(input$map_marker_dragend, {
      drag_info <- input$map_marker_dragend

      # Get the current points data
      current_points <- points()

      # Find the marker by the layerId (id of the marker)
      marker_index <- which(current_points$id == drag_info$id)
      old_lat <- current_points$lat[marker_index]
      old_lng <- current_points$lng[marker_index]

      # Update the current points with new coordinates
      current_points[marker_index, "lat"] <- drag_info$lat
      current_points[marker_index, "lng"] <- drag_info$lng

      # Add update flags
      current_points$lat_updated <- current_points$lat != old_lat
      current_points$lng_updated <- current_points$lng != old_lng

      # Update the points reactive dataframe with the new positions
      points(current_points)

      # Redraw the markers on the map with updated coordinates
      leaflet::leafletProxy("map") %>%
        leaflet::clearMarkers() %>%
        leaflet::addMarkers(data = points(),
                            lat = ~lat, lng = ~lng,
                            popup = ~paste("Lat:", lat, "<br>Lng:", lng),
                            layerId = ~id,
                            options = leaflet::markerOptions(draggable = TRUE))

      # Display updated coordinates
      output$coordinates <- shiny::renderPrint({
        points()
      })
    })
  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)
}
