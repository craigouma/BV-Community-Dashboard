# ------------------------------------------------------------------
# ** R/map_module.R **
# ------------------------------------------------------------------
# This file is a self-contained Shiny Module for the map.
# It defines:
# 1. `map_module_ui`: The UI function to render the map output.
# 2. `map_module_server`: The server function to render the map
#    and return the ID of a clicked shape.
# ------------------------------------------------------------------

# --- 1. Module UI Function ---
# This function creates the UI for the map.
# It is called from the main `ui.R` file.
map_module_ui <- function(id) {
    # `NS(id)` creates a "namespaced" ID (e.g., "map_1-map").
    # This is critical for preventing ID collisions in large apps.
    ns <- NS(id)
    
    tagList(
        # The leaflet map output [2, 33]
        leafletOutput(ns("map"), height = "600px")
    )
}

# --- 2. Module Server Function ---
# This function contains the server-side logic for the map.
# It is called from the main `server.R` file.
map_module_server <- function(id, fishing_data_r, mpa_data_r) {
    
    # `moduleServer` is the standard way to define a module's server logic.
    moduleServer(id, function(input, output, session) {
        
        # --- A. Render the Base Map ---
        # This runs once to create the initial map canvas.
        output$map <- renderLeaflet({
            leaflet() %>%
                addProviderTiles(providers$CartoDB.Positron, group = "Base Map") %>%
                addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
                setView(lng = -70, lat = 15, zoom = 6) # Placeholder view
        })
        
        # --- B. Add/Update MPA Polygons (Observer) ---
        # We use an `observe` block and `leafletProxy` to add data
        # to the map *without* redrawing the entire canvas. This is
        # a key performance optimization.[21]
        observe({
            req(mpa_data_r()) # Ensure the reactive data is loaded
            
            mpa_data <- mpa_data_r()
            
            leafletProxy("map", data = mpa_data) %>%
                clearShapes() %>% # Clear old shapes
                addPolygons(
                    fillColor = "orange",
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    fillOpacity = 0.5,
                    group = "MPA Boundaries",
                    highlightOptions = highlightOptions(
                        weight = 5,
                        color = "#666",
                        fillOpacity = 0.7,
                        bringToFront = TRUE
                    ),
                    # This is critical: assign a unique `layerId` to each polygon.
                    # This ID is what will be returned on click.
                    layerId = ~mpa_name, 
                    label = ~paste("MPA:", mpa_name)
                ) %>%
                # Add a layers control for map tiles and overlays
                addLayersControl(
                    baseGroups = c("Base Map", "Satellite"),
                    overlayGroups = c("MPA Boundaries", "Fishing Effort"),
                    options = layersControlOptions(collapsed = FALSE)
                )
        })
        
        # --- C. Add Fishing Data Points (Observer) ---
        observe({
            req(fishing_data_r())
            
            # For performance, we sample the data.
            # A more advanced app might use clustering.
            fishing_points <- fishing_data_r() %>% 
                sample_n(min(nrow(.), 2000)) # Sample for performance
                
            leafletProxy("map", data = fishing_points) %>%
                clearMarkers() %>% # Clear previous points
                addCircleMarkers(
                    lng = ~longitude,
                    lat = ~latitude,
                    radius = 2,
                    color = "#005aA3", # Blue Ventures blue
                    fillOpacity = 0.8,
                    stroke = FALSE,
                    group = "Fishing Effort",
                    popup = ~paste("Vessel:", vessel_id, "<br>",
                                 "Hours:", fishing_hours, "<br>",
                                 "Catch:", catch_kg, "kg")
                )
        })
        
        # --- D. Return Clicked MPA ID ---
        # This is the "output" of the module. We return a reactive
        # expression that contains the `layerId` of the polygon
        # that the user clicks. This is how we link the map to the plots.
        return(reactive({ input$map_shape_click$id }))
    })
}