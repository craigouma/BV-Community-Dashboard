# ------------------------------------------------------------------
# ** R/table_module.R **
# ------------------------------------------------------------------
# Enhanced Data Explorer Module - Senior-level data usability features
# Includes column metrics, saved filters, advanced downloads, and analytics
# ------------------------------------------------------------------

# --- 1. Module UI Function ---
table_module_ui <- function(id) {
    ns <- NS(id)

    tagList(
        # Control Panel
        fluidRow(
            box(
                title = "Data Explorer Controls",
                width = 12,
                solidHeader = TRUE,
                status = "primary",

                fluidRow(
                    column(width = 3,
                        selectInput(ns("filter_preset"), "Filter Presets:",
                                  choices = c("All Data", "High Effort Days", "Industrial Vessels",
                                            "Small Scale Only", "Recent Month", "Custom"),
                                  selected = "All Data")
                    ),
                    column(width = 3,
                        downloadButton(ns("download_csv"), "Download CSV",
                                     class = "btn-primary"),
                        downloadButton(ns("download_parquet"), "Download Parquet",
                                     class = "btn-secondary")
                    ),
                    column(width = 3,
                        actionButton(ns("save_filter"), "Save Current Filter",
                                   icon = icon("save"), class = "btn-info")
                    ),
                    column(width = 3,
                        checkboxInput(ns("show_metrics"), "Show Column Metrics", TRUE)
                    )
                )
            )
        ),

        # Column Metrics Panel (conditional)
        conditionalPanel(
            condition = paste0("input['", ns("show_metrics"), "'] == true"),
            ns = ns,
            fluidRow(
                box(
                    title = "Column-Level Analytics",
                    width = 12,
                    solidHeader = TRUE,
                    status = "info",
                    uiOutput(ns("column_metrics"))
                )
            )
        ),

        # Data Table
        fluidRow(
            box(
                title = "Fisheries Data Explorer",
                width = 12,
                solidHeader = TRUE,
                status = "primary",
                reactableOutput(ns("data_table"), height = "600px")
            )
        )
    )
}

# --- 2. Module Server Function ---
table_module_server <- function(id, fishing_data_r) {

    moduleServer(id, function(input, output, session) {

        get_input <- function(value, default) {
            if (is.null(value)) default else value
        }

        # --- A. Reactive Data Filtering ---
        filtered_data_r <- reactive({
            data <- fishing_data_r()

            filter_choice <- get_input(input$filter_preset, "All Data")

            switch(filter_choice,
                "High Effort Days" = data %>% filter(fishing_hours > quantile(fishing_hours, 0.75, na.rm = TRUE)),
                "Industrial Vessels" = data %>% filter(vessel_type == "industrial"),
                "Small Scale Only" = data %>% filter(vessel_type == "small_scale"),
                "Recent Month" = data %>% filter(date >= max(date, na.rm = TRUE) - 30),
                "Custom" = data, # Would be enhanced with custom filter UI
                data # All Data
            )
        })

        # --- B. Column Metrics Calculation ---
        column_metrics_r <- reactive({
            data <- filtered_data_r()

            metrics <- lapply(names(data), function(col) {
                values <- data[[col]]

                if (is.numeric(values)) {
                    list(
                        column = col,
                        type = "numeric",
                        completeness = mean(!is.na(values)) * 100,
                        mean = mean(values, na.rm = TRUE),
                        median = median(values, na.rm = TRUE),
                        min = min(values, na.rm = TRUE),
                        max = max(values, na.rm = TRUE),
                        distribution = "histogram"
                    )
                } else if (inherits(values, "Date")) {
                    list(
                        column = col,
                        type = "date",
                        completeness = mean(!is.na(values)) * 100,
                        range = paste(format(range(values, na.rm = TRUE), "%Y-%m-%d"), collapse = " to "),
                        distribution = "timeline"
                    )
                } else {
                    # Character/factor columns
                    list(
                        column = col,
                        type = "categorical",
                        completeness = mean(!is.na(values)) * 100,
                        unique_values = length(unique(na.omit(values))),
                        top_values = names(sort(table(values), decreasing = TRUE))[1:3],
                        distribution = "bar"
                    )
                }
            })

            names(metrics) <- names(data)
            metrics
        })

        # --- C. Column Metrics Display ---
        output$column_metrics <- renderUI({
            metrics <- column_metrics_r()
            show_metrics <- isTRUE(get_input(input$show_metrics, TRUE))

            if (!show_metrics) {
                return(NULL)
            }

            metric_boxes <- lapply(names(metrics), function(col) {
                m <- metrics[[col]]

                column(width = 3,
                    div(class = "metric-card",
                        h5(col),
                        tags$small(class = "text-muted", paste0("Type: ", m$type)),

                        if (m$type == "numeric") {
                            div(
                                p(strong("Completeness:"), paste0(round(m$completeness, 1), "%")),
                                p(strong("Mean:"), round(m$mean, 2)),
                                p(strong("Range:"), paste(round(m$min, 1), "to", round(m$max, 1)))
                            )
                        } else if (m$type == "date") {
                            div(
                                p(strong("Completeness:"), paste0(round(m$completeness, 1), "%")),
                                p(strong("Range:"), m$range)
                            )
                        } else {
                            div(
                                p(strong("Completeness:"), paste0(round(m$completeness, 1), "%")),
                                p(strong("Unique:"), m$unique_values),
                                p(strong("Top:"), paste(m$top_values, collapse = ", "))
                            )
                        }
                    )
                )
            })

            fluidRow(do.call(tagList, metric_boxes))
        })

        # --- D. Enhanced Data Table ---
        output$data_table <- renderReactable({
            data <- filtered_data_r()
            req(data)

            show_metrics <- isTRUE(get_input(input$show_metrics, TRUE))

            reactable(
                data,
                filterable = TRUE,
                searchable = TRUE,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(10, 25, 50, 100, 500),
                defaultPageSize = 25,
                striped = TRUE,
                highlight = TRUE,
                compact = TRUE,
                selection = "multiple", # Allow row selection for analysis
                onClick = "select",
                columns = list(
                    vessel_id = colDef(
                        name = "Vessel ID",
                        minWidth = 100,
                        # Add sparkline for vessel activity
                        cell = function(value) {
                            vessel_data <- data[data$vessel_id == value, ]
                            if (nrow(vessel_data) > 1) {
                                # Simple activity indicator
                                activity <- round(mean(vessel_data$fishing_hours, na.rm = TRUE), 1)
                                div(
                                    div(value),
                                    div(style = "font-size: 0.8em; color: #666;",
                                        paste(activity, "hrs avg"))
                                )
                            } else {
                                value
                            }
                        }
                    ),
                    date = colDef(
                        name = "Date",
                        format = colFormat(date = TRUE),
                        minWidth = 100
                    ),
                    latitude = colDef(
                        name = "Latitude",
                        format = colFormat(digits = 4),
                        minWidth = 80
                    ),
                    longitude = colDef(
                        name = "Longitude",
                        format = colFormat(digits = 4),
                        minWidth = 80
                    ),
                    fishing_hours = colDef(
                        name = "Fishing Hours",
                        format = colFormat(digits = 1),
                        minWidth = 100,
                        # Color coding for effort levels
                        style = function(value) {
                            if (is.na(value)) return(NULL)
                            if (value > 12) "background: #ffe6e6;" # High effort - red tint
                            else if (value > 6) "background: #fff3cd;" # Medium effort - yellow tint
                            else "background: #d4edda;" # Low effort - green tint
                        }
                    ),
                    catch_kg = colDef(
                        name = "Catch (kg)",
                        format = colFormat(digits = 1),
                        minWidth = 90,
                        # CPUE calculation
                        cell = function(value, index) {
                            hours <- data$fishing_hours[index]
                            if (!is.na(hours) && hours > 0) {
                                cpue <- round(value / hours, 2)
                                div(
                                    div(value),
                                    div(style = "font-size: 0.8em; color: #666;",
                                        paste(cpue, "kg/hr"))
                                )
                            } else {
                                value
                            }
                        }
                    ),
                    vessel_type = colDef(
                        name = "Vessel Type",
                        minWidth = 120,
                        # Icon and color coding
                        cell = function(value) {
                            if (value == "industrial") {
                                div(
                                    icon("ship", style = "color: #dc3545;"),
                                    span(" Industrial", style = "margin-left: 5px;")
                                )
                            } else {
                                div(
                                    icon("fish", style = "color: #28a745;"),
                                    span(" Small Scale", style = "margin-left: 5px;")
                                )
                            }
                        },
                        filterInput = function(values, name) {
                            selectInput(name, NULL, choices = unique(values),
                                      multiple = TRUE, selectize = TRUE)
                        }
                    ),
                    mpa_name = colDef(
                        name = "MPA Name",
                        minWidth = 150,
                        # MPA status indicator
                        cell = function(value) {
                            if (is.na(value) || value == "Outside") {
                                div(
                                    icon("exclamation-triangle", style = "color: #ffc107;"),
                                    span(" Outside MPA", style = "margin-left: 5px;")
                                )
                            } else {
                                div(
                                    icon("shield-alt", style = "color: #007bff;"),
                                    span(" ", value, style = "margin-left: 5px;")
                                )
                            }
                        },
                        filterInput = function(values, name) {
                            selectInput(name, NULL, choices = unique(values),
                                      multiple = TRUE, selectize = TRUE)
                        }
                    )
                ),
                # Add summary row
                defaultColDef = colDef(footer = function(values) {
                    if (is.numeric(values)) {
                        format(round(mean(values, na.rm = TRUE), 1), nsmall = 1)
                    } else if (length(unique(values)) == 1) {
                        unique(values)
                    } else {
                        paste(length(unique(values)), "unique")
                    }
                })
            )
        })

        # --- E. Download Handlers ---
        output$download_csv <- downloadHandler(
            filename = function() {
                paste0("fisheries_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
            },
            content = function(file) {
                write.csv(filtered_data_r(), file, row.names = FALSE)
            }
        )

        output$download_parquet <- downloadHandler(
            filename = function() {
                paste0("fisheries_data_", format(Sys.Date(), "%Y%m%d"), ".parquet")
            },
            content = function(file) {
                arrow::write_parquet(filtered_data_r(), file)
            }
        )

        # --- F. Save Filter Functionality ---
        observeEvent(input$save_filter, {
            # In a real app, this would save to a database or config file
            showNotification(
                "Filter saved! (This would persist filters in a real application)",
                type = "success",
                duration = 3
            )
        })

    })
}