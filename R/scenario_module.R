# ------------------------------------------------------------------
# ** R/scenario_module.R **
# ------------------------------------------------------------------
# Scenario Analysis Module - Decision Workflows feature
# Allows users to simulate policy impacts (season closures, gear bans)
# Provides real-time indicator updates and recommendations
# ------------------------------------------------------------------

# --- 1. Module UI Function ---
scenario_module_ui <- function(id) {
    ns <- NS(id)

    tagList(
        # Scenario Controls
        fluidRow(
            box(
                title = "Policy Scenario Controls",
                width = 12,
                solidHeader = TRUE,
                status = "warning",

                # Season Closure Toggle
                fluidRow(
                    column(width = 6,
                        checkboxInput(ns("season_closure"), "Season Closure (June-August)", FALSE),
                        conditionalPanel(
                            condition = paste0("input['", ns("season_closure"), "'] == true"),
                            sliderInput(ns("closure_reduction"), "Effort Reduction %", 0, 100, 70, step = 10),
                            ns = ns
                        )
                    ),
                    column(width = 6,
                        checkboxInput(ns("gear_ban"), "Industrial Gear Ban", FALSE),
                        conditionalPanel(
                            condition = paste0("input['", ns("gear_ban"), "'] == true"),
                            sliderInput(ns("ban_effectiveness"), "Ban Effectiveness %", 0, 100, 80, step = 10),
                            ns = ns
                        )
                    )
                ),

                # Alert Thresholds
                fluidRow(
                    column(width = 6,
                        numericInput(ns("effort_threshold"), "Effort Alert Threshold (%)",
                                   value = 25, min = 0, max = 100)
                    ),
                    column(width = 6,
                        numericInput(ns("cpue_threshold"), "CPUE Alert Threshold (%)",
                                   value = 20, min = 0, max = 100)
                    )
                )
            )
        ),

        # Scenario Impact Results
        fluidRow(
            valueBoxOutput(ns("scenario_effort_impact"), width = 3),
            valueBoxOutput(ns("scenario_cpue_impact"), width = 3),
            valueBoxOutput(ns("scenario_compliance_impact"), width = 3),
            valueBoxOutput(ns("scenario_health_impact"), width = 3)
        ),

        # Before/After Comparison
        fluidRow(
            box(
                title = "Scenario Impact Analysis",
                width = 12,
                solidHeader = TRUE,
                status = "info",
                plotlyOutput(ns("scenario_comparison_plot"), height = "400px")
            )
        ),

        # Recommendations Panel
        fluidRow(
            box(
                title = "Policy Recommendations",
                width = 12,
                solidHeader = TRUE,
                status = "success",
                uiOutput(ns("scenario_recommendations"))
            )
        )
    )
}

# --- 2. Module Server Function ---
scenario_module_server <- function(id, fishing_data_r, selected_mpa_id_r) {

    moduleServer(id, function(input, output, session) {

        # Helper to safely read inputs even before UI is rendered
        get_input <- function(value, default) {
            if (is.null(value)) default else value
        }

        # --- A. Baseline Metrics Calculation ---
        baseline_metrics_r <- reactive({
            req(fishing_data_r())

            data <- fishing_data_r()

            # Filter for selected MPA if applicable
            selected_mpa <- selected_mpa_id_r()
            if (!is.null(selected_mpa) && selected_mpa != "All Areas") {
                data <- data %>% filter(mpa_name == selected_mpa)
            }

            # Calculate baseline monthly metrics
            data %>%
                mutate(month = floor_date(date, "month")) %>%
                group_by(month) %>%
                summarise(
                    total_effort = sum(fishing_hours, na.rm = TRUE),
                    total_catch = sum(catch_kg, na.rm = TRUE),
                    cpue = ifelse(total_effort > 0, total_catch / total_effort, 0),
                    industrial_effort = sum(fishing_hours[vessel_type == "industrial"], na.rm = TRUE),
                    small_scale_effort = sum(fishing_hours[vessel_type == "small_scale"], na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                mutate(
                    industrial_ratio = industrial_effort / total_effort,
                    scenario = "Baseline"
                )
        })

        # --- B. Scenario Simulation ---
        scenario_metrics_r <- reactive({
            baseline <- baseline_metrics_r()

            season_closure_active <- isTRUE(get_input(input$season_closure, FALSE))
            closure_reduction <- get_input(input$closure_reduction, 70)
            gear_ban_active <- isTRUE(get_input(input$gear_ban, FALSE))
            ban_effectiveness <- get_input(input$ban_effectiveness, 80)

            # Apply season closure if enabled
            if (season_closure_active) {
                closure_months <- month(baseline$month) %in% c(6, 7, 8) # June, July, August
                baseline <- baseline %>%
                    mutate(
                        total_effort = ifelse(closure_months,
                                            total_effort * (1 - closure_reduction/100),
                                            total_effort),
                        industrial_effort = ifelse(closure_months,
                                                 industrial_effort * (1 - closure_reduction/100),
                                                 industrial_effort),
                        small_scale_effort = ifelse(closure_months,
                                                  small_scale_effort * (1 - closure_reduction/100),
                                                  small_scale_effort)
                    )
            }

            # Apply gear ban if enabled
            if (gear_ban_active) {
                baseline <- baseline %>%
                    mutate(
                        industrial_effort = industrial_effort * (1 - ban_effectiveness/100),
                        total_effort = industrial_effort + small_scale_effort,
                        industrial_ratio = industrial_effort / total_effort
                    )
            }

            # Recalculate CPUE with new effort levels
            baseline %>%
                mutate(
                    cpue = ifelse(total_effort > 0, total_catch / total_effort, 0),
                    scenario = "With Policy"
                )
        })

        # --- C. Impact Calculations ---
        scenario_impacts_r <- reactive({
            baseline <- baseline_metrics_r()
            scenario <- scenario_metrics_r()

            # Calculate average impacts
            baseline_avg <- baseline %>%
                summarise(
                    avg_effort = mean(total_effort, na.rm = TRUE),
                    avg_cpue = mean(cpue, na.rm = TRUE),
                    avg_industrial_ratio = mean(industrial_ratio, na.rm = TRUE),
                    .groups = "drop"
                )

            scenario_avg <- scenario %>%
                summarise(
                    avg_effort = mean(total_effort, na.rm = TRUE),
                    avg_cpue = mean(cpue, na.rm = TRUE),
                    avg_industrial_ratio = mean(industrial_ratio, na.rm = TRUE),
                    .groups = "drop"
                )

            # Calculate percentage changes
            impacts <- data.frame(
                effort_change = ((scenario_avg$avg_effort - baseline_avg$avg_effort) / baseline_avg$avg_effort) * 100,
                cpue_change = ((scenario_avg$avg_cpue - baseline_avg$avg_cpue) / baseline_avg$avg_cpue) * 100,
                compliance_change = ((baseline_avg$avg_industrial_ratio - scenario_avg$avg_industrial_ratio) / baseline_avg$avg_industrial_ratio) * 100
            )

            # Calculate health score changes
            max_effort_baseline <- max(baseline$total_effort, na.rm = TRUE)
            if (!is.finite(max_effort_baseline)) max_effort_baseline <- baseline_avg$avg_effort
            max_cpue_baseline <- max(baseline$cpue, na.rm = TRUE)
            if (!is.finite(max_cpue_baseline)) max_cpue_baseline <- baseline_avg$avg_cpue

            baseline_health <- 100 * (0.4 * (1 - pmin(baseline_avg$avg_effort / max_effort_baseline, 1)) +
                                     0.4 * pmin(baseline_avg$avg_cpue / max_cpue_baseline, 1) +
                                     0.2 * (1 - baseline_avg$avg_industrial_ratio))

            max_effort_scenario <- max(scenario$total_effort, na.rm = TRUE)
            if (!is.finite(max_effort_scenario)) max_effort_scenario <- scenario_avg$avg_effort
            max_cpue_scenario <- max(scenario$cpue, na.rm = TRUE)
            if (!is.finite(max_cpue_scenario)) max_cpue_scenario <- scenario_avg$avg_cpue

            scenario_health <- 100 * (0.4 * (1 - pmin(scenario_avg$avg_effort / max_effort_scenario, 1)) +
                                     0.4 * pmin(scenario_avg$avg_cpue / max_cpue_scenario, 1) +
                                     0.2 * (1 - scenario_avg$avg_industrial_ratio))

            impacts$health_change <- scenario_health - baseline_health

            impacts
        })

        # --- D. Impact Value Boxes ---
        output$scenario_effort_impact <- renderValueBox({
            impacts <- scenario_impacts_r()
            change <- round(impacts$effort_change, 1)
            effort_threshold <- get_input(input$effort_threshold, 25)
            color <- ifelse(abs(change) > effort_threshold, "red",
                           ifelse(change < 0, "green", "yellow"))

            valueBox(
                value = paste0(ifelse(change > 0, "+", ""), change, "%"),
                subtitle = "Effort Change",
                icon = icon(ifelse(change < 0, "arrow-down", "arrow-up")),
                color = color
            )
        })

        output$scenario_cpue_impact <- renderValueBox({
            impacts <- scenario_impacts_r()
            change <- round(impacts$cpue_change, 1)
            cpue_threshold <- get_input(input$cpue_threshold, 20)
            color <- ifelse(abs(change) > cpue_threshold, "red",
                           ifelse(change > 0, "green", "yellow"))

            valueBox(
                value = paste0(ifelse(change > 0, "+", ""), change, "%"),
                subtitle = "CPUE Change",
                icon = icon(ifelse(change > 0, "arrow-up", "arrow-down")),
                color = color
            )
        })

        output$scenario_compliance_impact <- renderValueBox({
            impacts <- scenario_impacts_r()
            change <- round(impacts$compliance_change, 1)

            valueBox(
                value = paste0(ifelse(change > 0, "+", ""), change, "%"),
                subtitle = "Compliance Improvement",
                icon = icon("balance-scale"),
                color = ifelse(change > 0, "green", "yellow")
            )
        })

        output$scenario_health_impact <- renderValueBox({
            impacts <- scenario_impacts_r()
            change <- round(impacts$health_change, 1)

            valueBox(
                value = paste0(ifelse(change > 0, "+", ""), change, " pts"),
                subtitle = "Health Score Change",
                icon = icon(ifelse(change > 0, "heartbeat", "exclamation-triangle")),
                color = ifelse(change > 0, "green", "red")
            )
        })

        # --- E. Scenario Comparison Plot ---
        output$scenario_comparison_plot <- renderPlotly({
            baseline <- baseline_metrics_r() %>% mutate(scenario = "Baseline")
            scenario <- scenario_metrics_r()

            # Combine data for plotting
            comparison_data <- bind_rows(baseline, scenario) %>%
                mutate(month = as.Date(month))

            p <- ggplot(comparison_data, aes(x = month, color = scenario, linetype = scenario)) +
                geom_line(aes(y = total_effort), size = 1) +
                scale_color_manual(values = c("Baseline" = "#dc3545", "With Policy" = "#28a745")) +
                scale_linetype_manual(values = c("Baseline" = "dashed", "With Policy" = "solid")) +
                labs(
                    title = "Policy Scenario Impact: Fishing Effort",
                    x = "Month",
                    y = "Total Fishing Hours",
                    color = "Scenario",
                    linetype = "Scenario"
                ) +
                theme_minimal() +
                theme(legend.position = "top")

            ggplotly(p, tooltip = c("x", "y", "colour"))
        })

        # --- F. Scenario Recommendations ---
        output$scenario_recommendations <- renderUI({
            impacts <- scenario_impacts_r()
            has_season_closure <- isTRUE(get_input(input$season_closure, FALSE))
            has_gear_ban <- isTRUE(get_input(input$gear_ban, FALSE))
            closure_reduction <- get_input(input$closure_reduction, 70)

            recommendations <- list()

            # Effort impact recommendations
            effort_threshold <- get_input(input$effort_threshold, 25)
            cpue_threshold <- get_input(input$cpue_threshold, 20)

            if (abs(impacts$effort_change) > effort_threshold) {
                if (impacts$effort_change < -20) {
                    recommendations <- c(recommendations,
                        "⚠️ Strong effort reduction detected - monitor for over-compliance or displacement to nearby areas")
                } else if (impacts$effort_change > 20) {
                    recommendations <- c(recommendations,
                        "🚨 Effort increase despite policy - investigate compliance gaps or enforcement issues")
                }
            }

            # CPUE impact recommendations
            if (impacts$cpue_change > cpue_threshold) {
                recommendations <- c(recommendations,
                    "🐟 Positive CPUE response - policy showing early signs of stock recovery")
            } else if (impacts$cpue_change < -10) {
                recommendations <- c(recommendations,
                    "📉 CPUE decline - consider additional complementary measures")
            }

            # Policy-specific recommendations
            if (has_season_closure && closure_reduction >= 70) {
                recommendations <- c(recommendations,
                    "🏖️ High season closure effectiveness - consider permanent no-take zones in key areas")
            }

            if (has_gear_ban && impacts$compliance_change > 50) {
                recommendations <- c(recommendations,
                    "🎣 Gear ban highly effective - expand to additional industrial vessels if feasible")
            }

            # Overall policy recommendation
            if (impacts$health_change > 10) {
                recommendations <- c(recommendations,
                    "✅ Policy simulation shows strong positive impact - recommend implementation")
            } else if (impacts$health_change < -5) {
                recommendations <- c(recommendations,
                    "❌ Policy simulation shows negative impact - reconsider approach or parameters")
            } else {
                recommendations <- c(recommendations,
                    "🤔 Policy impact uncertain - conduct pilot program before full implementation")
            }

            if (length(recommendations) == 0) {
                recommendations <- c("📊 Run different policy scenarios to see potential impacts")
            }

            HTML(paste("<ul>", paste("<li>", recommendations, "</li>", collapse = ""), "</ul>"))
        })

    })
}
