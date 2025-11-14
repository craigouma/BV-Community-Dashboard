# ------------------------------------------------------------------
# ** server.R **
# ------------------------------------------------------------------
# This is the main server logic file.
# In a modularized application , this file acts as a
# "director" or "router". It does not contain any complex logic itself.
# Instead, its job is to:
# 1. Initialize the core data as reactive objects.
# 2. Call the server-side function for each module.
# 3. Manage the flow of reactive data *between* modules.
#
# This "router" pattern keeps the app logic clean and maintainable,
# even as the application grows in complexity.
# ------------------------------------------------------------------

server <- function(input, output, session) {
    
    # --- 1. Initialize Core Data as Reactives ---
    core_data_r <- reactive({ fishing_data })
    mpa_data_r <- reactive({ mpa_boundaries })

    # --- 2. Initialize Map Module ---
    selected_mpa_id_r <- map_module_server(
        "map_1", 
        fishing_data_r = core_data_r, 
        mpa_data_r = mpa_data_r
    )
    
    # --- 3. Initialize Plot Module ---
    plot_module_server(
        "plot_1",
        fishing_data_r = core_data_r,
        selected_mpa_id_r = selected_mpa_id_r
    )
    
    # --- 4. Health Score Calculations ---
    health_metrics_r <- reactive({
        req(core_data_r(), mpa_data_r())

        data <- core_data_r()
        mpa_data <- mpa_data_r()

        selected_mpa <- selected_mpa_id_r()
        if (!is.null(selected_mpa) && selected_mpa != "All Areas") {
            data <- data %>% filter(mpa_name == selected_mpa)
        }

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
                effort_score = 100 * (1 - pmin(total_effort / max(total_effort, na.rm = TRUE), 1)),
                cpue_score = 100 * pmin(cpue / max(cpue, na.rm = TRUE), 1),
                compliance_score = 100 * (1 - industrial_ratio),
                health_score = (effort_score * 0.4 + cpue_score * 0.4 + compliance_score * 0.2)
            )

        overall_metrics <- monthly_metrics %>%
            summarise(
                avg_effort_score = mean(effort_score, na.rm = TRUE),
                avg_cpue_score = mean(cpue_score, na.rm = TRUE),
                avg_compliance_score = mean(compliance_score, na.rm = TRUE),
                avg_health_score = mean(health_score, na.rm = TRUE),
                trend_effort = ifelse(n() > 1, coef(lm(effort_score ~ as.numeric(month)))[2], 0),
                trend_cpue = ifelse(n() > 1, coef(lm(cpue_score ~ as.numeric(month)))[2], 0),
                .groups = "drop"
            )

        list(monthly = monthly_metrics, overall = overall_metrics)
    })

    # --- 5. Health Score Value Boxes ---
    output$overall_health_score <- renderValueBox({
        req(health_metrics_r())
        metrics <- health_metrics_r()
        score <- round(as.numeric(metrics$overall$avg_health_score), 1)
        color <- ifelse(score >= 70, "green", ifelse(score >= 50, "yellow", "red"))
        valueBox(value = paste0(score, "/100"), subtitle = "Overall Health Score",
                icon = icon("heartbeat"), color = color)
    })

    output$effort_reduction_score <- renderValueBox({
        req(health_metrics_r())
        metrics <- health_metrics_r()
        score <- round(as.numeric(metrics$overall$avg_effort_score), 1)
        trend <- as.numeric(metrics$overall$trend_effort)
        valueBox(value = paste0(score, "/100"), subtitle = "Effort Reduction",
                icon = icon(ifelse(trend > 0, "arrow-up", "arrow-down")), color = "blue")
    })

    output$cpue_recovery_score <- renderValueBox({
        req(health_metrics_r())
        metrics <- health_metrics_r()
        score <- round(as.numeric(metrics$overall$avg_cpue_score), 1)
        trend <- as.numeric(metrics$overall$trend_cpue)
        valueBox(value = paste0(score, "/100"), subtitle = "CPUE Recovery",
                icon = icon(ifelse(trend > 0, "arrow-up", "arrow-down")), color = "teal")
    })

    output$compliance_score <- renderValueBox({
        req(health_metrics_r())
        metrics <- health_metrics_r()
        score <- round(as.numeric(metrics$overall$avg_compliance_score), 1)
        valueBox(value = paste0(score, "/100"), subtitle = "Compliance",
                icon = icon("balance-scale"), color = "purple")
    })

    # --- 6. Conservation Insights ---
    output$conservation_insights <- renderUI({
        req(health_metrics_r())
        overall <- health_metrics_r()$overall

        insights <- list()
        health_score <- as.numeric(overall$avg_health_score)

        if (health_score >= 80) {
            insights <- c(insights, "🟢 Excellent conservation outcomes - LMMA showing strong recovery")
        } else if (health_score >= 60) {
            insights <- c(insights, "🟡 Moderate progress - continued monitoring needed")
        } else {
            insights <- c(insights, "🔴 Concerning trends - immediate action recommended")
        }

        if (as.numeric(overall$trend_effort) > 0) {
            insights <- c(insights, "📈 Fishing effort increasing - review compliance")
        }

        HTML(paste("<ul>", paste("<li>", insights, "</li>", collapse = ""), "</ul>"))
    })

    # --- 7. Priority Actions ---
    output$priority_actions <- renderUI({
        req(health_metrics_r())
        overall <- health_metrics_r()$overall

        actions <- list()
        health_score <- as.numeric(overall$avg_health_score)

        if (health_score < 60) {
            actions <- c(actions, "🚨 Schedule community meeting", "📋 Review regulations", "🔍 Conduct assessment")
        } else {
            actions <- c(actions, "📊 Continue monitoring", "🎯 Focus education", "📈 Expand no-take zones")
        }

        HTML(paste("<ul>", paste("<li>", actions, "</li>", collapse = ""), "</ul>"))
    })

    # --- 8. Trend Analysis ---
    output$trend_analysis <- renderUI({
        req(health_metrics_r())
        overall <- health_metrics_r()$overall

        effort_trend <- as.numeric(overall$trend_effort)
        cpue_trend <- as.numeric(overall$trend_cpue)

        trend_items <- list()

        # Effort trend analysis
        if (effort_trend > 0.5) {
            trend_items <- c(trend_items, "📈 Effort increasing significantly - potential overfishing")
        } else if (effort_trend < -0.5) {
            trend_items <- c(trend_items, "📉 Effort decreasing - positive conservation impact")
        } else {
            trend_items <- c(trend_items, "➡️ Effort stable - monitoring required")
        }

        # CPUE trend analysis
        if (cpue_trend > 0.3) {
            trend_items <- c(trend_items, "🐟 CPUE improving - fish stocks recovering")
        } else if (cpue_trend < -0.3) {
            trend_items <- c(trend_items, "⚠️ CPUE declining - stock depletion risk")
        } else {
            trend_items <- c(trend_items, "➡️ CPUE stable - sustainable levels maintained")
        }

        HTML(paste("<ul>", paste("<li>", trend_items, "</li>", collapse = ""), "</ul>"))
    })

    # --- 9. Risk Assessment ---
    output$risk_assessment <- renderUI({
        req(health_metrics_r())
        overall <- health_metrics_r()$overall

        health_score <- as.numeric(overall$avg_health_score)
        compliance_score <- as.numeric(overall$avg_compliance_score)

        risk_level <- ifelse(health_score < 50, "🔴 HIGH RISK",
                           ifelse(health_score < 70, "🟡 MEDIUM RISK", "🟢 LOW RISK"))

        risk_factors <- list()

        if (compliance_score < 70) {
            risk_factors <- c(risk_factors, "⚠️ Low compliance - enforcement needed")
        }

        if (as.numeric(overall$trend_effort) > 0) {
            risk_factors <- c(risk_factors, "⚠️ Increasing fishing pressure")
        }

        if (as.numeric(overall$trend_cpue) < 0) {
            risk_factors <- c(risk_factors, "⚠️ Declining catch rates")
        }

        if (length(risk_factors) == 0) {
            risk_factors <- c("✅ No major risk factors identified")
        }

        HTML(paste0("<strong>Risk Level: ", risk_level, "</strong><br><br>",
                   paste("<ul>", paste("<li>", risk_factors, "</li>", collapse = ""), "</ul>")))
    })

    # --- 10. Conservation Timeline ---
    output$conservation_timeline <- renderUI({
        req(health_metrics_r())

        # Calculate some timeline metrics
        data <- core_data_r()
        date_range <- range(data$date, na.rm = TRUE)
        months_active <- length(unique(format(data$date, "%Y-%m")))

        timeline_items <- list()
        timeline_items <- c(timeline_items,
            paste("📅 Monitoring period:", format(date_range[1], "%b %Y"), "to", format(date_range[2], "%b %Y")),
            paste("📊 Active months:", months_active),
            paste("🎯 Fishing observations:", format(nrow(data), big.mark = ",")),
            "📈 Trend analysis updated monthly"
        )

        HTML(paste("<ul>", paste("<li>", timeline_items, "</li>", collapse = ""), "</ul>"))
    })

    # --- 11. Data Quality Metrics ---
    data_quality_r <- reactive({
        req(core_data_r())
        data <- core_data_r()

        completeness <- mean(!is.na(data)) * 100
        accuracy <- 95  # Simulated accuracy score

        list(completeness = completeness, accuracy = accuracy)
    })

    output$data_completeness <- renderUI({
        score <- data_quality_r()$completeness
        tags$div(style = paste0("color: ", ifelse(score >= 95, "green", "orange"), "; font-size: 18px; font-weight: bold;"),
                paste0(round(score, 1), "%"))
    })

    output$data_accuracy <- renderUI({
        score <- data_quality_r()$accuracy
        tags$div(style = paste0("color: ", ifelse(score >= 95, "green", "orange"), "; font-size: 18px; font-weight: bold;"),
                paste0(round(score, 1), "%"))
    })

    output$completeness_progress <- renderUI({
        score <- data_quality_r()$completeness
        color <- ifelse(score >= 95, "success", "warning")
        tags$div(class = paste0("progress-bar bg-", color), style = paste0("width: ", score, "%"),
                paste0(round(score, 1), "%"))
    })

    output$accuracy_progress <- renderUI({
        score <- data_quality_r()$accuracy
        color <- ifelse(score >= 95, "success", "warning")
        tags$div(class = paste0("progress-bar bg-", color), style = paste0("width: ", score, "%"),
                paste0(round(score, 1), "%"))
    })

    output$total_records <- renderInfoBox({
        infoBox("Total Records", format(nrow(core_data_r()), big.mark = ","),
               icon = icon("database"), color = "blue")
    })

    output$date_coverage <- renderInfoBox({
        dates <- range(core_data_r()$date, na.rm = TRUE)
        date_range <- paste(format(dates[1], "%b %Y"), "-", format(dates[2], "%b %Y"))
        infoBox("Date Coverage", date_range, icon = icon("calendar"), color = "green")
    })

    # --- 12. Data Explorer ---
    filtered_data_r <- reactive({
        data <- core_data_r()
        switch(input$filter_preset,
            "High Effort Days" = data %>% filter(fishing_hours > quantile(fishing_hours, 0.75, na.rm = TRUE)),
            "Industrial Vessels Only" = data %>% filter(vessel_type == "industrial"),
            "Recent Activity" = data %>% filter(date >= max(date, na.rm = TRUE) - 30),
            "MPA Fishing" = data %>% filter(!is.na(mpa_name)),
            data
        )
    })

    output$data_table_overview <- renderReactable({
        req(filtered_data_r())
        data <- filtered_data_r()

        reactable(data,
                 filterable = TRUE,
                 searchable = TRUE,
                 pageSizeOptions = c(10, 25, 50),
                 defaultPageSize = 10,
                 striped = TRUE,
                 columns = list(
                     date = colDef(format = colFormat(date = TRUE)),
                     fishing_hours = colDef(format = colFormat(digits = 1)),
                     catch_kg = colDef(format = colFormat(digits = 1)),
                     latitude = colDef(format = colFormat(digits = 3)),
                     longitude = colDef(format = colFormat(digits = 3))
                 ))
    })

    output$download_data <- downloadHandler(
        filename = function() { paste0("fisheries_data_", format(Sys.Date(), "%Y%m%d"), ".csv") },
        content = function(file) { write.csv(filtered_data_r(), file, row.names = FALSE) }
    )

    # --- 13. Policy Scenario Analysis ---
    observeEvent(input$run_scenario, {
        # Show analysis running notification
        showNotification(
            "Running policy scenario analysis...",
            type = "message",
            duration = 2
        )

        # Run scenario analysis based on current inputs
        data <- core_data_r()

        # Apply season closure if enabled
        if (!is.null(input$season_closure) && input$season_closure) {
            closure_months <- month(data$date) %in% c(6, 7, 8) # June, July, August
            data <- data %>%
                mutate(fishing_hours = ifelse(closure_months,
                    fishing_hours * (1 - input$closure_reduction/100),
                    fishing_hours))
        }

        # Apply gear ban if enabled
        if (!is.null(input$gear_ban) && input$gear_ban) {
            data <- data %>%
                mutate(fishing_hours = ifelse(vessel_type == "industrial",
                    fishing_hours * (1 - input$ban_effectiveness/100),
                    fishing_hours))
        }

        # Calculate baseline metrics
        baseline <- core_data_r() %>%
            summarise(
                total_effort = sum(fishing_hours, na.rm = TRUE),
                total_catch = sum(catch_kg, na.rm = TRUE),
                avg_cpue = total_catch / total_effort,
                industrial_ratio = mean(vessel_type == "industrial", na.rm = TRUE),
                .groups = "drop"
            )

        # Calculate scenario metrics
        scenario <- data %>%
            summarise(
                total_effort = sum(fishing_hours, na.rm = TRUE),
                total_catch = sum(catch_kg, na.rm = TRUE),
                avg_cpue = total_catch / total_effort,
                industrial_ratio = mean(vessel_type == "industrial", na.rm = TRUE),
                .groups = "drop"
            )

        # Calculate percentage changes
        effort_change <- round(((scenario$total_effort - baseline$total_effort) / baseline$total_effort) * 100, 1)
        cpue_change <- round(((scenario$avg_cpue - baseline$avg_cpue) / baseline$avg_cpue) * 100, 1)
        compliance_change <- round(((baseline$industrial_ratio - scenario$industrial_ratio) / baseline$industrial_ratio) * 100, 1)

        # Calculate health score change (simplified)
        baseline_health <- 100 * (0.4 * (1 - baseline$total_effort/max(baseline$total_effort, 1e-10)) +
                                  0.4 * baseline$avg_cpue/max(baseline$avg_cpue, 1e-10) +
                                  0.2 * (1 - baseline$industrial_ratio))

        scenario_health <- 100 * (0.4 * (1 - scenario$total_effort/max(scenario$total_effort, 1e-10)) +
                                  0.4 * scenario$avg_cpue/max(scenario$avg_cpue, 1e-10) +
                                  0.2 * (1 - scenario$industrial_ratio))

        health_change <- round(scenario_health - baseline_health, 1)

        # Show results notification
        results_text <- sprintf(
            "Policy Impact Analysis Complete:\n• Effort Change: %s%%\n• CPUE Change: %s%%\n• Compliance Improvement: %s%%\n• Health Score Change: %+.1f pts",
            effort_change, cpue_change, compliance_change, health_change
        )

        showNotification(
            results_text,
            type = "message",
            duration = 8
        )

        # Also show alerts based on thresholds
        effort_threshold <- if(!is.null(input$effort_threshold)) input$effort_threshold else 25
        cpue_threshold <- if(!is.null(input$cpue_threshold)) input$cpue_threshold else 20

        if (abs(effort_change) > effort_threshold) {
            showNotification(
                sprintf("⚠️ ALERT: Effort change (%s%%) exceeds threshold (%s%%)", effort_change, effort_threshold),
                type = "warning",
                duration = 6
            )
        }

        if (abs(cpue_change) > cpue_threshold) {
            showNotification(
                sprintf("⚠️ ALERT: CPUE change (%s%%) exceeds threshold (%s%%)", cpue_change, cpue_threshold),
                type = "warning",
                duration = 6
            )
        }
    })

}