# ------------------------------------------------------------------
# ** R/health_module.R **
# ------------------------------------------------------------------
# LMMA Health Score Module - Senior-level mission alignment feature
# Calculates health scores combining effort, CPUE, and compliance metrics
# Provides visual indicators for conservation impact assessment
# ------------------------------------------------------------------

# --- 1. Module UI Function ---
health_module_ui <- function(id) {
    ns <- NS(id)

    tagList(
        # Health Score Cards Row
        fluidRow(
            valueBoxOutput(ns("health_score_overall"), width = 3),
            valueBoxOutput(ns("health_score_effort"), width = 3),
            valueBoxOutput(ns("health_score_cpue"), width = 3),
            valueBoxOutput(ns("health_score_compliance"), width = 3)
        ),

        # Health Trends Chart
        fluidRow(
            box(
                title = "LMMA Health Trends",
                width = 12,
                solidHeader = TRUE,
                status = "success",
                plotlyOutput(ns("health_trends_plot"), height = "300px")
            )
        ),

        # Key Insights Panel
        fluidRow(
            box(
                title = "Conservation Insights",
                width = 6,
                solidHeader = TRUE,
                status = "info",
                uiOutput(ns("insights_panel"))
            ),
            box(
                title = "Recommendations",
                width = 6,
                solidHeader = TRUE,
                status = "warning",
                uiOutput(ns("recommendations_panel"))
            )
        )
    )
}

# --- 2. Module Server Function ---
health_module_server <- function(id, fishing_data_r, mpa_data_r, selected_mpa_id_r) {

    moduleServer(id, function(input, output, session) {

        # Debug: Log module initialization
        cat("Health module server initialized with ID:", id, "\n")

        # --- A. Calculate Health Metrics ---
        health_metrics_r <- reactive({
            cat("Health metrics reactive triggered\n")

            req(fishing_data_r(), mpa_data_r())
            cat("Required data available\n")

            data <- fishing_data_r()
            mpa_data <- mpa_data_r()

            cat("Data dimensions - fishing:", nrow(data), "rows,", ncol(data), "cols\n")
            cat("MPA data dimensions:", nrow(mpa_data), "rows\n")

            # Get selected MPA or use all data
            selected_mpa <- selected_mpa_id_r()
            cat("Selected MPA:", selected_mpa, "\n")

            if (!is.null(selected_mpa) && selected_mpa != "All Areas") {
                data <- data %>% filter(mpa_name == selected_mpa)
                mpa_info <- mpa_data %>% filter(mpa_name == selected_mpa)
                cat("Filtered data to MPA:", selected_mpa, "- remaining rows:", nrow(data), "\n")
            } else {
                mpa_info <- mpa_data
                selected_mpa <- "All Areas"
                cat("Using all data (All Areas)\n")
            }

            # Calculate monthly metrics
            monthly_metrics <- data %>%
                mutate(month = floor_date(date, "month")) %>%
                group_by(month, mpa_name) %>%
                summarise(
                    total_effort = sum(fishing_hours, na.rm = TRUE),
                    total_catch = sum(catch_kg, na.rm = TRUE),
                    cpue = ifelse(total_effort > 0, total_catch / total_effort, 0),
                    vessel_count = n_distinct(vessel_id),
                    industrial_ratio = mean(vessel_type == "industrial", na.rm = TRUE),
                    .groups = "drop"
                ) %>%
                mutate(
                    # Effort Score (lower effort = healthier)
                    effort_score = 100 * (1 - pmin(total_effort / max(total_effort, na.rm = TRUE), 1)),

                    # CPUE Score (higher CPUE = healthier recovery)
                    cpue_score = 100 * pmin(cpue / max(cpue, na.rm = TRUE), 1),

                    # Compliance Score (lower industrial ratio = better compliance)
                    compliance_score = 100 * (1 - industrial_ratio),

                    # Overall Health Score (weighted average)
                    health_score = (effort_score * 0.4 + cpue_score * 0.4 + compliance_score * 0.2)
                )

            # Calculate overall metrics
            overall_metrics <- monthly_metrics %>%
                summarise(
                    avg_effort_score = mean(effort_score, na.rm = TRUE),
                    avg_cpue_score = mean(cpue_score, na.rm = TRUE),
                    avg_compliance_score = mean(compliance_score, na.rm = TRUE),
                    avg_health_score = mean(health_score, na.rm = TRUE),
                    trend_effort = ifelse(n() > 1, coef(lm(effort_score ~ as.numeric(month)))[2], 0),
                    trend_cpue = ifelse(n() > 1, coef(lm(cpue_score ~ as.numeric(month)))[2], 0),
                    trend_health = ifelse(n() > 1, coef(lm(health_score ~ as.numeric(month)))[2], 0),
                    .groups = "drop"
                )

            result <- list(
                monthly = monthly_metrics,
                overall = overall_metrics,
                selected_area = selected_mpa
            )
            cat("Health metrics calculation completed successfully\n")
            result
        })

        # --- B. Health Score Value Boxes ---
        output$health_score_overall <- renderValueBox({
            cat("Rendering health_score_overall valueBox\n")
            req(health_metrics_r())
            metrics <- health_metrics_r()
            overall_data <- metrics[["overall"]]
            score <- round(as.numeric(overall_data[["avg_health_score"]]), 1)
            cat("Overall health score:", score, "\n")
            color <- ifelse(score >= 70, "green",
                           ifelse(score >= 50, "yellow", "red"))

            valueBox(
                value = paste0(score, "/100"),
                subtitle = "Overall Health Score",
                icon = icon("heartbeat"),
                color = color
            )
        })

        output$health_score_effort <- renderValueBox({
            req(health_metrics_r())
            metrics <- health_metrics_r()
            overall_data <- metrics[["overall"]]
            score <- round(as.numeric(overall_data[["avg_effort_score"]]), 1)
            trend <- as.numeric(overall_data[["trend_effort"]])

            valueBox(
                value = paste0(score, "/100"),
                subtitle = "Effort Reduction",
                icon = icon(ifelse(trend > 0, "arrow-up", "arrow-down")),
                color = ifelse(score >= 70, "green", "yellow")
            )
        })

        output$health_score_cpue <- renderValueBox({
            req(health_metrics_r())
            metrics <- health_metrics_r()
            overall_data <- metrics[["overall"]]
            score <- round(as.numeric(overall_data[["avg_cpue_score"]]), 1)
            trend <- as.numeric(overall_data[["trend_cpue"]])

            valueBox(
                value = paste0(score, "/100"),
                subtitle = "CPUE Recovery",
                icon = icon(ifelse(trend > 0, "arrow-up", "arrow-down")),
                color = ifelse(score >= 70, "green", "yellow")
            )
        })

        output$health_score_compliance <- renderValueBox({
            req(health_metrics_r())
            metrics <- health_metrics_r()
            overall_data <- metrics[["overall"]]
            score <- round(as.numeric(overall_data[["avg_compliance_score"]]), 1)

            valueBox(
                value = paste0(score, "/100"),
                subtitle = "Compliance",
                icon = icon("balance-scale"),
                color = ifelse(score >= 70, "green", "yellow")
            )
        })

        # --- C. Health Trends Plot ---
        output$health_trends_plot <- renderPlotly({
            cat("Rendering health_trends_plot\n")
            req(health_metrics_r())
            metrics <- health_metrics_r()

            cat("Plot - monthly data rows:", nrow(metrics$monthly), "\n")
            cat("Plot - selected area:", metrics$selected_area, "\n")

            p <- ggplot(metrics$monthly, aes(x = month)) +
                geom_line(aes(y = health_score, color = "Overall Health"), size = 1.5) +
                geom_line(aes(y = effort_score, color = "Effort Reduction"), alpha = 0.7) +
                geom_line(aes(y = cpue_score, color = "CPUE Recovery"), alpha = 0.7) +
                geom_line(aes(y = compliance_score, color = "Compliance"), alpha = 0.7) +
                scale_color_manual(values = c(
                    "Overall Health" = "#28a745",
                    "Effort Reduction" = "#007bff",
                    "CPUE Recovery" = "#17a2b8",
                    "Compliance" = "#6f42c1"
                )) +
                labs(
                    title = paste("Health Score Trends -", metrics$selected_area),
                    x = "Month",
                    y = "Health Score (0-100)",
                    color = "Metric"
                ) +
                theme_minimal() +
                theme(legend.position = "bottom")

            cat("Plot created successfully\n")
            ggplotly(p, tooltip = c("x", "y", "colour"))
        })

        # --- D. Insights Panel ---
        output$insights_panel <- renderUI({
            cat("Rendering insights_panel\n")
            req(health_metrics_r())
            metrics <- health_metrics_r()
            overall_data <- metrics[["overall"]]

            insights <- list()

            # Health score insights
            health_score <- as.numeric(overall_data[["avg_health_score"]])
            cat("Insights - health score:", health_score, "\n")
            if (health_score >= 80) {
                insights <- c(insights, "🟢 Excellent conservation outcomes - LMMA showing strong recovery")
            } else if (health_score >= 60) {
                insights <- c(insights, "🟡 Moderate progress - continued monitoring needed")
            } else {
                insights <- c(insights, "🔴 Concerning trends - immediate action recommended")
            }

            # Effort trend insights
            effort_trend <- as.numeric(overall_data[["trend_effort"]])
            cat("Insights - effort trend:", effort_trend, "\n")
            if (effort_trend > 0) {
                insights <- c(insights, "📈 Fishing effort increasing - review compliance measures")
            } else if (effort_trend < -0.5) {
                insights <- c(insights, "📉 Fishing effort decreasing - positive conservation impact")
            }

            # CPUE insights
            cpue_trend <- as.numeric(overall_data[["trend_cpue"]])
            cat("Insights - CPUE trend:", cpue_trend, "\n")
            if (cpue_trend > 0) {
                insights <- c(insights, "🐟 CPUE improving - fish stocks showing recovery signs")
            } else {
                insights <- c(insights, "⚠️ CPUE stable/declining - monitor fishing pressure")
            }

            cat("Generated", length(insights), "insights\n")
            HTML(paste("<ul>", paste("<li>", insights, "</li>", collapse = ""), "</ul>"))
        })

        # --- E. Recommendations Panel ---
        output$recommendations_panel <- renderUI({
            cat("Rendering recommendations_panel\n")
            req(health_metrics_r())
            metrics <- health_metrics_r()
            overall_data <- metrics[["overall"]]

            recommendations <- list()

            health_score <- as.numeric(overall_data[["avg_health_score"]])
            cat("Recommendations - health score:", health_score, "\n")
            if (health_score < 60) {
                recommendations <- c(recommendations,
                    "🚨 High Priority: Schedule community meeting to discuss conservation measures",
                    "📋 Review and strengthen enforcement of fishing regulations",
                    "🔍 Conduct fisheries assessment to identify pressure points"
                )
            } else if (health_score < 80) {
                recommendations <- c(recommendations,
                    "📊 Continue monitoring trends and compliance",
                    "🎯 Focus community education on sustainable fishing practices",
                    "📈 Consider expanding no-take zones in high-pressure areas"
                )
            } else {
                recommendations <- c(recommendations,
                    "✅ Maintain current conservation measures - excellent results",
                    "📊 Document success story for other communities",
                    "🎯 Consider mentoring neighboring communities"
                )
            }

            cat("Generated", length(recommendations), "recommendations\n")
            HTML(paste("<ul>", paste("<li>", recommendations, "</li>", collapse = ""), "</ul>"))
        })

    })
}
