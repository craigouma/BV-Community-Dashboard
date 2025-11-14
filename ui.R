# ------------------------------------------------------------------
# ** ui.R **
# ------------------------------------------------------------------
# This is the main User Interface (UI) file.
# It defines the overall layout of the dashboard using `shinydashboard`.[26, 27]
#
# A key principle of this "senior" architecture is that this file
# is minimal. It only defines the *layout*. The actual UI
# content (maps, plots, tables) is encapsulated in modules.
# This makes the UI structure easy to read and manage.
# ------------------------------------------------------------------

ui <- dashboardPage(
    skin = "blue", # Sets the color theme

    # --- 1. Header ---
    # Defines the top navigation bar.
    dashboardHeader(
        title = "Blue Ventures Fisheries Intelligence Platform",
        titleWidth = 350 # Width must match the sidebar
    ),

    # --- 2. Sidebar ---
    # Defines the main navigation menu on the left.
    dashboardSidebar(
        width = 350,
        collapsed = TRUE, # Sidebar starts collapsed
        sidebarMenu(
            id = "sidebar_tabs", # An ID for the menu

            # --- Comprehensive Overview Dashboard ---
            menuItem(
                "Blue Ventures Fisheries Dashboard",
                tabName = "dashboard",
                icon = icon("dashboard")
            )
        )
    ),
    
    # --- 3. Body ---
    # Defines the main content area. The content is organized
    # into `tabItems` that correspond to the `tabName` from the
    # `menuItem`s in the sidebar.
    dashboardBody(
        tabItems(

            # --- Comprehensive Dashboard Content ---
            tabItem(
                tabName = "dashboard",

                # --- Key Metrics & Health Scores ---
                fluidRow(
                    # LMMA Health Scores
                    valueBoxOutput("overall_health_score", width = 3),
                    valueBoxOutput("effort_reduction_score", width = 3),
                    valueBoxOutput("cpue_recovery_score", width = 3),
                    valueBoxOutput("compliance_score", width = 3)
                ),

                # --- Map  ---
                fluidRow(
                    box(
                        title = "MPA & Fishing Effort Map",
                        width = 12,
                        solidHeader = TRUE,
                        status = "primary",
                        height = "500px",
                        map_module_ui("map_1")
                    )
                ),

                # --- Time Series Analysis Only ---
                fluidRow(
                    box(
                        title = "Time Series Analysis",
                        width = 12,
                        solidHeader = TRUE,
                        status = "primary",
                        height = "400px",
                        plot_module_ui("plot_1")
                    )
                ),

                # --- Conservation Intelligence | Policy Scenario Analysis ---
                fluidRow(
                    # Conservation Intelligence
                    box(
                        title = "Conservation Intelligence",
                        width = 6,
                        solidHeader = TRUE,
                        status = "success",
                        height = "650px",
                        uiOutput("conservation_insights"),
                        hr(),
                        h4("Priority Actions"),
                        uiOutput("priority_actions"),
                        hr(),
                        h4("Trend Analysis"),
                        uiOutput("trend_analysis"),
                        hr(),
                        h4("Risk Assessment"),
                        uiOutput("risk_assessment"),
                        hr(),
                        h4("Conservation Timeline"),
                        uiOutput("conservation_timeline")
                    ),

                    # Policy Scenario Analysis
                    box(
                        title = "Policy Scenario Analysis",
                        width = 6,
                        solidHeader = TRUE,
                        status = "warning",
                        height = "650px",

                        # Season Closure Toggle
                        checkboxInput("season_closure", "Season Closure (June-August)", FALSE),
                        conditionalPanel(
                            condition = "input.season_closure == true",
                            sliderInput("closure_reduction", "Effort Reduction %", 0, 100, 70, step = 10)
                        ),

                        hr(),

                        # Gear Ban Toggle
                        checkboxInput("gear_ban", "Industrial Gear Ban", FALSE),
                        conditionalPanel(
                            condition = "input.gear_ban == true",
                            sliderInput("ban_effectiveness", "Ban Effectiveness %", 0, 100, 80, step = 10)
                        ),

                        hr(),

                        # Alert Thresholds
                        h5("Alert Thresholds"),
                        numericInput("effort_threshold", "Effort Change Alert (%)", value = 25, min = 0, max = 100),
                        numericInput("cpue_threshold", "CPUE Change Alert (%)", value = 20, min = 0, max = 100),

                        hr(),
                        actionButton("run_scenario", "Run Policy Analysis", icon = icon("calculator"),
                                   class = "btn-primary btn-lg", style = "width: 100%;"),
                        helpText("Click to analyze the impact of selected policy scenarios on fisheries metrics")
                    )
                ),

                # --- Data Quality Dashboard ---
                fluidRow(
                    box(
                        title = "Data Quality Dashboard",
                        width = 12,
                        solidHeader = TRUE,
                        status = "info",
                        height = "200px",

                        # Completeness | Accuracy | Total Records | Date Coverage
                        fluidRow(
                            column(width = 3,
                                h5("Completeness"),
                                uiOutput("data_completeness"),
                                progressBar("completeness_progress", value = 0, display_pct = TRUE)
                            ),
                            column(width = 3,
                                h5("Accuracy"),
                                uiOutput("data_accuracy"),
                                progressBar("accuracy_progress", value = 0, display_pct = TRUE)
                            ),
                            column(width = 3,
                                infoBoxOutput("total_records", width = 12)
                            ),
                            column(width = 3,
                                infoBoxOutput("date_coverage", width = 12)
                            )
                        )
                    )
                ),

                # --- Fisheries Data Explorer ---
                fluidRow(
                    box(
                        title = "Fisheries Data Explorer",
                        width = 12,
                        solidHeader = TRUE,
                        status = "primary",
                        height = "700px",

                        # Control Panel
                        fluidRow(
                            column(width = 3,
                                selectInput("filter_preset", "Quick Filters:",
                                          choices = c("All Data", "High Effort Days", "Industrial Vessels Only",
                                                    "Recent Activity", "MPA Fishing"), selected = "All Data")
                            ),
                            column(width = 3,
                                downloadButton("download_data", "Export Data", class = "btn-primary")
                            ),
                            column(width = 3,
                                checkboxInput("show_advanced", "Show Column Metrics", FALSE)
                            ),
                            column(width = 3,
                                actionButton("refresh_data", "Refresh", icon = icon("sync"), class = "btn-info")
                            )
                        ),

                        # Data Table
                        reactableOutput("data_table_overview", height = "550px")
                    )
                )
            )
        )
    )
)