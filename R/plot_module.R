# ------------------------------------------------------------------
# ** R/plot_module.R **
# ------------------------------------------------------------------
# This module encapsulates the `plotly` charts.
# It is designed to be *reactive* to the map module.
# It receives `selected_mpa_id_r` as an argument, which it uses
# to filter the data before plotting.
# ------------------------------------------------------------------

# --- 1. Module UI Function ---
plot_module_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        # A dynamic title that will update based on the map selection
        h4(textOutput(ns("plot_title"))),
        
        # Two plots side-by-side
        fluidRow(
            column(width = 6,
                # Plotly output for the time series [34]
                plotlyOutput(ns("effort_timeseries_plot"), height = "300px")
            ),
            column(width = 6,
                plotlyOutput(ns("cpue_timeseries_plot"), height = "300px")
            )
        )
    )
}

# --- 2. Module Server Function ---
plot_module_server <- function(id, fishing_data_r, selected_mpa_id_r) {
    
    moduleServer(id, function(input, output, session) {
        
        # --- A. Create Filtered Data Reactive ---
        # This reactive expression is the core logic. It listens to
        # both the main data (`fishing_data_r`) and the map click
        # (`selected_mpa_id_r`).
        filtered_data_r <- reactive({
            req(fishing_data_r())
            
            data <- fishing_data_r()
            
            # This is the key link!
            selected_mpa <- selected_mpa_id_r()
            
            if (is.null(selected_mpa) || selected_mpa == "All Areas") {
                # If no MPA is clicked, analyze all data
                list(
                    data = data,
                    area_name = "All Areas"
                )
            } else {
                # If an MPA is clicked, filter for that area
                list(
                    data = data %>% filter(mpa_name == selected_mpa),
                    area_name = selected_mpa
                )
            }
        })
        
        # --- B. Render the Dynamic Title ---
        output$plot_title <- renderText({
            paste("Showing Analysis for:", filtered_data_r()$area_name)
        })
        
        # --- C. Prepare Summarized Plot Data ---
        plot_data_r <- reactive({
            req(filtered_data_r())
            
            filtered_data_r()$data %>%
                mutate(month = floor_date(date, "month")) %>%
                group_by(month) %>%
                summarise(
                    total_effort = sum(fishing_hours, na.rm = TRUE),
                    total_catch = sum(catch_kg, na.rm = TRUE),
                    # Calculate CPUE (Catch Per Unit Effort)
                    cpue = total_catch / total_effort
                ) %>%
                # Handle division by zero if effort is 0
                mutate(cpue = ifelse(is.infinite(cpue), NA, cpue))
        })

        # --- D. Render the Plotly Charts ---
        output$effort_timeseries_plot <- renderPlotly({
            req(plot_data_r())
            
            p <- ggplot(plot_data_r(), aes(x = month, y = total_effort)) +
                geom_line(color = "#005aA3") +
                geom_point(color = "#005aA3") +
                labs(
                    title = "Monthly Fishing Effort",
                    x = "Month",
                    y = "Total Fishing Hours"
                ) +
                theme_minimal()
            
            # Convert to plotly for interactivity [34, 35]
            ggplotly(p, tooltip = c("x", "y"))
        })

        output$cpue_timeseries_plot <- renderPlotly({
            req(plot_data_r())

            p <- ggplot(plot_data_r(), aes(x = month, y = cpue)) +
                geom_line(color = "#E05A10") +
                geom_point(color = "#E05A10") +
                labs(
                    title = "Monthly Catch Per Unit Effort (CPUE)",
                    x = "Month",
                    y = "Catch (kg) / Hour"
                ) +
                theme_minimal()
            
            ggplotly(p, tooltip = c("x", "y"))
        })
    })
}