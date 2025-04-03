#' Interactive Point Snapping on Map
#'
#' This function allows users to interactively snap points on a map by clicking
#' and dragging markers. It returns a data frame with old and new coordinates,
#' and indicates whether the coordinates were updated.
#'
#' @param coords_df A data frame containing columns `lat` and `lng` for latitude and longitude.
#' @return A data frame with old and new coordinates and update status.
#' @import shiny
#' @import leaflet
#' @import terra
#' @import sf
#' @import leaflet.extras
#' @import miniUI
#'
#' @examples
#'\dontrun{
#' coords <- data.frame(
#' id = 1:2,
#' lat = c(48.2082, 44.8176),   # Vienna, Belgrade
#' lng = c(16.3738, 20.4633))    # Vienna, Belgrade
#' updated_coords <- snap_points_on_map(coords)
#' }
#'
#' @export

snap_points_on_map <- function(coords_df) {

  # Ensure input data frame has required columns
  if (!all(c("lat", "lng") %in% names(coords_df))) {
    stop("Input data frame must contain 'lat' and 'lng' columns.")
  }

  # Add an ID column if not present
  if (!"id" %in% names(coords_df)) {
    coords_df$id <- seq_len(nrow(coords_df))
  }

  # Define the miniUI
  mini_ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Drag Points to Update Coordinates"),
    miniUI::miniContentPanel(
      leaflet::leafletOutput("map", width = "100%", height = "600px"),
      shiny::fileInput("file", "Upload Raster/Vector File (GeoTIFF/GeoPackage)", accept = c(".tif", ".gpkg")),
      shiny::tags$style(shiny::HTML("#file {margin-top: 10px;}"))
    )
  )

  # Define server logic
  server <- function(input, output, session) {

    # Reactive dataframe to store points
    points <- shiny::reactiveVal(coords_df)

    # Reactive to store uploaded layer
    uploaded_layer <- reactiveVal(NULL)

    # Track whether a raster or vector layer is loaded
    layer_type <- reactiveVal(NULL)

    # Render leaflet map with draggable markers
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles("Esri.WorldTopoMap") |>
        leaflet::addMarkers(data = points(),
                            lat = ~lat, lng = ~lng,
                            popup = ~paste("Lat:", lat, "<br>Lng:", lng),
                            layerId = ~id,
                            options = leaflet::markerOptions(draggable = TRUE)) |>
        leaflet::addLayersControl(
          overlayGroups = c("Uploaded Layer"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )  # Add layer control
    })

    # Update coordinates after dragging markers
    shiny::observeEvent(input$map_marker_dragend, {
      drag_info <- input$map_marker_dragend

      # Get the current points data
      current_points <- points()

      # Find the marker by the layerId (id of the marker)
      marker_index <- which(current_points$id == drag_info$id)

      # Update the current points with new coordinates
      current_points[marker_index, "lat"] <- drag_info$lat
      current_points[marker_index, "lng"] <- drag_info$lng

      # Update the points reactive dataframe
      points(current_points)

      # Redraw the markers without removing the uploaded layer
      leaflet_proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearMarkers() |>
        leaflet::addMarkers(data = points(),
                            lat = ~lat, lng = ~lng,
                            popup = ~paste("Lat:", lat, "<br>Lng:", lng),
                            layerId = ~id,
                            options = leaflet::markerOptions(draggable = TRUE))

      # Ensure that the uploaded layer remains visible
      if (!is.null(uploaded_layer())) {
        if (layer_type() == "raster") {
          leaflet_proxy <- leaflet_proxy |>
            leaflet::addRasterImage(uploaded_layer(), group = "Uploaded Layer", opacity = 0.7)
        } else if (layer_type() == "vector") {
          leaflet_proxy <- leaflet_proxy |>
            leaflet::addPolygons(data = uploaded_layer(), color = "blue", group = "Uploaded Layer")
        }

        leaflet_proxy |>
          leaflet::addLayersControl(
            overlayGroups = c("Uploaded Layer"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )
      }
    })

    # Handle file upload and add raster/vector layer to the map
    observeEvent(input$file, {
      req(input$file)

      # Read the uploaded file
      file_path <- input$file$datapath
      file_ext <- tools::file_ext(input$file$name)

      # Handle raster and vector files differently
      if (file_ext %in% c("tif", "tiff")) {
        # Raster file
        layer <- terra::rast(file_path)
        uploaded_layer(layer)
        layer_type("raster")

        # Add raster to the map
        leaflet::leafletProxy("map") |>
          leaflet::addRasterImage(layer, group = "Uploaded Layer", opacity = 0.7) |>
          leaflet::addLayersControl(
            overlayGroups = c("Uploaded Layer"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )
      } else if (file_ext == "gpkg") {
        # GeoPackage vector file
        layer <- sf::st_read(file_path)
        uploaded_layer(layer)
        layer_type("vector")

        # Add vector (GeoPackage) layer to the map
        leaflet::leafletProxy("map") |>
          leaflet::addPolygons(data = layer, color = "blue", group = "Uploaded Layer") |>
          leaflet::addLayersControl(
            overlayGroups = c("Uploaded Layer"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )
      } else {
        shiny::showNotification("Invalid file type. Please upload a GeoTIFF or GeoPackage file.", type = "error")
      }
    })

    # Save the updated data frame and close the gadget
    shiny::observeEvent(input$done, {
      updated_data <- points() |>
        dplyr::rename(new_lat = lat,
                      new_lng = lng) |>
        dplyr::mutate(original_lat = coords_df$lat,
                      original_lng = coords_df$lng)
      shiny::stopApp(updated_data)  # Close the app and return updated data
    })

  }

  # Run the application
  shiny::runGadget(mini_ui, server)
}
