# ==============================================================================
# Project: Bean Counting 3.5 - Restoration of Strategic Insight Logic
# Author: SU Bo-Han (Cooper)
# ==============================================================================

pacman::p_load(
  shiny, bslib, tidyverse, corrplot, plotly, DT, scales, 
  ggstatsplot, shinyWidgets, rpart, rpart.plot, networkD3, 
  data.tree, shinydashboard, shinycustomloader, bsicons, caret, ranger
)

df_raw <- read_csv("colombia_customers_processed.csv", show_col_types = FALSE) %>%
  mutate(
    across(where(is.character), as.factor),
    log_total_tx_volume = log1p(total_tx_volume),
    log_tx_count = log1p(tx_count),
    log_avg_tx_value = log1p(avg_tx_value)
  )

var_labels <- c(
  churn_probability = "Churn Probability", customer_tenure = "Customer Tenure",
  satisfaction_score = "Satisfaction Score", total_tx_volume = "Total Transaction Volume",
  tx_count = "Transaction Count", avg_tx_value = "Average Transaction Value"
)
label_var <- function(x) ifelse(x %in% names(var_labels), var_labels[x], x)
ml_vars <- c("churn_probability", "age", "occupation", "income_bracket", "active_products", 
             "tx_count", "customer_tenure", "failed_transactions", "credit_utilization_ratio", 
             "satisfaction_score", "complaint_topics")

# ==============================================================================
# UI DESIGN
# ==============================================================================

ui <- page_navbar(
  title = "More Than Just Beans",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#C5A381", secondary = "#8D6E63"),
  
  header = tags$head(
    tags$style(HTML("
      .navbar { background-color: #C5A381 !important; }
      .btn-milktea { background-color: #C5A381 !important; border-color: #B28F6F !important; color: white !important; font-weight: 600 !important; transition: all 0.3s ease; }
      .card-header { background-color: #E3D5CA !important; color: #5D4037 !important; font-weight: bold; }
      .active-strat-card { border-radius: 15px; padding: 25px; text-align: center; border: 3px solid #C5A381; background: #ffffff; box-shadow: 0 6px 20px rgba(0,0,0,0.15); }
      .insight-box { background: #FDF5E6; padding: 20px; border-radius: 10px; border-left: 5px solid #C5A381; margin-top: 10px; }
      .nav-tabs .nav-link.active { background-color: #E3D5CA !important; border-color: #C5A381 !important; color: #5D4037 !important; font-weight: bold; }
    "))
  ),
  
  # --- 1. EDA Explorer ---
  nav_panel("📊 EDA Explorer",
            layout_columns(
              value_box("Total Customers", nrow(df_raw), showcase = bs_icon("people"), theme = "secondary"),
              value_box("Avg Churn Risk", percent(mean(df_raw$churn_probability)), showcase = bs_icon("graph-down"), theme = "danger"),
              value_box("Avg Satisfaction", round(mean(df_raw$satisfaction_score), 2), showcase = bs_icon("emoji-smile"), theme = "info")
            ),
            navset_card_pill(
              nav_panel("Distribution", sidebarLayout(sidebarPanel(selectInput("dist_var", "Variable:", choices = c("churn_probability", "customer_tenure", "satisfaction_score", "active_products", "log_total_tx_volume")), selectInput("dist_segment", "CLV Segment:", choices = c("All", sort(unique(as.character(df_raw$clv_segment))))), sliderInput("dist_bins", "Bins:", 10, 60, 30), actionButton("run_dist", "Update Distribution", class="btn-milktea w-100")), mainPanel(plotlyOutput("distPlot")))),
              nav_panel("Relationship", sidebarLayout(sidebarPanel(selectInput("rel_var", "Compare:", choices = c("active_products", "satisfaction_score", "customer_tenure")), selectInput("rel_chart_type", "Type:", choices = c("Boxplot", "Violin Plot", "Mean Plot")), checkboxInput("facet_segment", "Facet Segment"), actionButton("run_rel", "Update Relationship", class="btn-milktea w-100")), mainPanel(plotlyOutput("relPlot")))),
              nav_panel("Correlation", sidebarLayout(sidebarPanel(selectizeInput("corr_vars", "Variables:", choices = names(df_raw)[sapply(df_raw, is.numeric)], selected = c("active_products", "tx_count", "satisfaction_score", "churn_probability"), multiple = TRUE), sliderInput("corr_threshold", "Threshold:", 0, 1, 0.2), actionButton("run_corr", "Update Correlation", class="btn-milktea w-100")), mainPanel(plotOutput("corrPlot"), DTOutput("corrTable")))),
              nav_panel("Segment Profiler", sidebarLayout(sidebarPanel(checkboxGroupInput("profile_metrics", "Metrics:", choices = c("customer_lifetime_value", "total_tx_volume", "satisfaction_score", "churn_probability"), selected = c("total_tx_volume", "churn_probability")), checkboxInput("standardize_profile", "Standardize"), actionButton("run_profile", "Update Profiler", class="btn-milktea w-100")), mainPanel(plotlyOutput("profilePlot"))))
            )
  ),
  
  # --- 2. Group Analysis ---
  nav_panel("⚖️ Group Analysis",
            sidebarLayout(
              sidebarPanel(checkboxGroupInput("conf_gender", "Gender:", choices = c("Male", "Female", "Other"), selected = c("Male", "Female", "Other")), pickerInput("conf_location", "Location:", choices = sort(unique(as.character(df_raw$location))), multiple = TRUE, options = list(`actions-box` = TRUE), selected = unique(as.character(df_raw$location))[1:3]), actionButton("run_conf", "Run Test", class="btn-milktea w-100")),
              mainPanel(layout_columns(card(card_header("Visualization"), plotOutput("conf_plot")), card(card_header("Config"), selectInput("conf_x", "X:", choices = c("income_bracket", "customer_segment")), selectInput("conf_y", "Y:", choices = c("churn_probability", "satisfaction_score")))))
            )
  ),
  
  # --- 3. 🎯 Strategic Clustering ---
  nav_panel("🎯 Strategic Clustering",
            sidebarLayout(
              sidebarPanel(
                sliderInput("clust_k", "Clusters (k):", 2, 8, 3),
                h5("Strategic Weights", style="font-weight: bold; color: #8D6E63;"),
                sliderInput("w_vol", "Volume Weight (Revenue):", 0, 1, 0.6, step = 0.1),
                sliderInput("w_risk", "Risk Weight (Churn):", 0, 1, 0.3, step = 0.1),
                actionButton("run_clust", "Execute Analysis", class = "btn-milktea w-100", icon = icon("sync")),
                br(), br(),
                span("Note: High Volume + High Risk weight = Defensive mode.", style = "font-size: 0.8rem; color: gray;")
              ),
              mainPanel(
                uiOutput("priority_boxes"),
                hr(),
                fluidRow(
                  column(width = 4, uiOutput("active_strategy_card")), 
                  column(width = 8, uiOutput("strategic_insight_text")) 
                ),
                hr(),
                layout_columns(
                  card(card_header("Cluster Behavioral Map"), plotlyOutput("cluster_scatter", height = "400px")),
                  card(card_header("Segment Personas Summary"), DTOutput("persona_table"))
                )
              )
            )
  ),
  
  # --- 4. 🌲 Decision Tree ---
  nav_panel("🌲 Decision Tree",
            sidebarLayout(
              sidebarPanel(sliderInput("dt_cp", "CP:", 0.0001, 0.05, 0.001), sliderInput("dt_depth", "Depth:", 1, 15, 7), actionButton("run_dt", "Generate Tree", class="btn-milktea w-100")),
              mainPanel(layout_column_wrap(width = 1/2, card(card_header("Decision Path"), diagonalNetworkOutput("dt_plot")), card(card_header("CP Plot"), plotOutput("dt_cp_plot"))))
            )
  ),
  
  # --- 5. 🦊 Random Forest ---
  nav_panel("🦊 Random Forest",
            sidebarLayout(
              sidebarPanel(sliderInput("rf_trees", "Trees:", 10, 200, 50), actionButton("run_rf", "Train Model", class="btn-milktea w-100")),
              mainPanel(
                layout_columns(value_box("Model R-Squared", uiOutput("rf_r2_val"), showcase = bs_icon("check2-circle"), theme = "success"), value_box("Mean Absolute Error", uiOutput("rf_mae_val"), showcase = bs_icon("graph-up-arrow"), theme = "warning")),
                layout_column_wrap(width = 1/2, card(card_header("Accuracy"), plotlyOutput("rf_scatter")), card(card_header("Importance"), plotOutput("rf_imp"))),
                hr(),
                h3("🔍 Churn Simulator", class = "sim-header"),
                card(card_body(layout_column_wrap(width = 1/2, div(h5("Input Features"), numericInput("sim_age", "Age:", 35), selectInput("sim_occ", "Occupation:", choices = sort(unique(as.character(df_raw$occupation)))), numericInput("sim_active", "Active Products:", 2), sliderInput("sim_sat", "Satisfaction:", 1, 6, 3), actionButton("predict_sim", "Predict Risk Now", class="btn-milktea w-100")), div(h5("Assessment"), uiOutput("sim_result_ui")))))
              )
            )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # EDA 邏輯
  dist_plot_obj <- eventReactive(input$run_dist, { df <- if(input$dist_segment == "All") df_raw else df_raw %>% filter(clv_segment == input$dist_segment); ggplot(df, aes(x = .data[[input$dist_var]])) + geom_histogram(bins = input$dist_bins, fill = "#C5A381", color = "white") + theme_minimal() }, ignoreNULL = FALSE)
  output$distPlot <- renderPlotly({ ggplotly(dist_plot_obj()) })
  
  rel_plot_obj <- eventReactive(input$run_rel, { var <- input$rel_var; df <- df_raw; if (input$rel_chart_type == "Boxplot") { p <- ggplot(df, aes(x = factor(.data[[var]]), y = churn_probability, fill = factor(.data[[var]]))) + geom_boxplot(alpha = 0.7) + scale_fill_brewer(palette = "BrBG") } else if (input$rel_chart_type == "Violin Plot") { p <- ggplot(df, aes(x = factor(.data[[var]]), y = churn_probability, fill = factor(.data[[var]]))) + geom_violin(alpha = 0.7) + geom_boxplot(width=0.1) + scale_fill_brewer(palette = "BrBG") } else { summary_df <- df %>% group_by(g = .data[[var]]) %>% summarise(m = mean(churn_probability)); p <- ggplot(summary_df, aes(x = factor(g), y = m, fill = factor(g))) + geom_col() + scale_fill_brewer(palette = "BrBG") }; if(input$facet_segment) p <- p + facet_wrap(~clv_segment); p + theme_minimal() }, ignoreNULL = FALSE)
  output$relPlot <- renderPlotly({ ggplotly(rel_plot_obj()) })
  
  corr_data <- eventReactive(input$run_corr, { req(length(input$corr_vars) >= 2); cor(df_raw %>% select(all_of(input$corr_vars)), use = "pairwise.complete.obs") }, ignoreNULL = FALSE)
  output$corrPlot <- renderPlot({ req(corr_data()); corrplot(corr_data(), method = "color", type = "upper", tl.col = "black", col = colorRampPalette(c("#8D6E63", "white", "#C5A381"))(200)) })
  output$corrTable <- renderDT({ req(corr_data()); as.data.frame(as.table(corr_data())) %>% filter(Var1 != Var2) %>% mutate(abs_c = abs(Freq)) %>% filter(abs_c >= input$corr_threshold) %>% arrange(desc(abs_c)) %>% mutate(across(where(is.numeric), ~round(.x, 3))) %>% datatable(options = list(pageLength = 5), rownames = FALSE) })
  
  profile_plot_obj <- eventReactive(input$run_profile, { req(input$profile_metrics); prof_df <- df_raw %>% group_by(clv_segment) %>% summarise(across(all_of(input$profile_metrics), mean)) %>% pivot_longer(-clv_segment); if(input$standardize_profile) prof_df <- prof_df %>% group_by(name) %>% mutate(value = as.numeric(scale(value))); ggplot(prof_df, aes(x = clv_segment, y = value, fill = clv_segment)) + geom_col() + facet_wrap(~name, scales = "free_y") + scale_fill_manual(values = c("#D7CCC8", "#BCAAA4", "#8D6E63", "#C5A381")) + theme_minimal() }, ignoreNULL = FALSE)
  output$profilePlot <- renderPlotly({ ggplotly(profile_plot_obj()) })
  
  # --- Clustering  ---
  clust_res <- eventReactive(input$run_clust, {
    set.seed(42)
    df_s <- df_raw %>% sample_n(min(nrow(.), 5000))
    df_c <- df_s %>% mutate(v = scale(log10(total_tx_volume + 1)), r = scale(churn_probability)) %>% select(v, r)
    km <- kmeans(df_c, centers = input$clust_k, nstart = 25)
    list(data = df_s %>% mutate(cluster = as.factor(km$cluster)))
  }, ignoreNULL = FALSE)
  
 
  current_mode <- reactive({
    v_h <- input$w_vol > 0.5; r_h <- input$w_risk > 0.5
    if (v_h && r_h) return(list(m="Premium", i="gem", d="Elite Growth Focus"))
    if (v_h && !r_h) return(list(m="Aggressive", i="rocket", d="Revenue Expansion"))
    if (!v_h && r_h) return(list(m="Defensive", i="shield-alt", d="Risk Mitigation"))
    return(list(m="Balanced", i="balance-scale", d="Market Stability"))
  })
  

  priority_data <- reactive({
    req(clust_res())
    clust_res()$data %>% 
      group_by(occupation) %>%
      summarise(count = n(), avg_vol = mean(total_tx_volume, na.rm = TRUE), 
                avg_sat = mean(satisfaction_score, na.rm = TRUE), avg_churn = mean(churn_probability, na.rm = TRUE)) %>%
      filter(count > 10) %>%
      mutate(score = as.numeric((input$w_vol * scale(log10(avg_vol + 1))) + (0.2 * scale(avg_sat)) - (input$w_risk * scale(avg_churn)))) %>%
      arrange(desc(score)) %>% head(3)
  })
  
  output$priority_boxes <- renderUI({
    req(priority_data())
    d <- priority_data(); labels <- c("🥇 Rank 1", "🥈 Rank 2", "🥉 Rank 3"); colors <- c("yellow", "teal", "orange")
    fluidRow(lapply(seq_len(nrow(d)), function(i) {
      column(width = 4, infoBox(labels[i], d$occupation[i], subtitle = paste("Strategic Score:", round(d$score[i], 2)), icon = icon("award"), color = colors[i], fill = TRUE, width = NULL))
    }))
  })
  
  output$active_strategy_card <- renderUI({
    mode <- current_mode()
    div(class = "active-strat-card", icon(mode$i, style="font-size: 50px; color: #C5A381;"), h3(mode$m), p(mode$d))
  })
  
  output$strategic_insight_text <- renderUI({
    req(priority_data())
    d <- priority_data(); mode <- current_mode()$m; top_occ <- d$occupation[1]; sec_occ <- d$occupation[2]
    tagList(
      h3(paste(mode, "Strategy Execution Insights"), style="color: #5D4037; font-weight: bold;"),
      div(class="insight-box",
          p(strong("Target Segment: "), paste0("The '", top_occ, "' segment is your highest priority.")),
          tags$ul(
            tags$li(strong("Managerial Action: "), paste("Launch high-value retention campaigns specifically for", top_occ)),
            tags$li(strong("Growth Potential: "), paste("Monitor", sec_occ, "for potential service upgrades."))
          )
      )
    )
  })
  
  output$cluster_scatter <- renderPlotly({ req(clust_res()); p <- ggplot(clust_res()$data, aes(x = total_tx_volume, y = churn_probability, color = cluster)) + geom_point(alpha = 0.5) + scale_x_log10(labels = comma) + theme_minimal() + scale_color_brewer(palette = "BrBG"); ggplotly(p) })
  output$persona_table <- renderDT({ req(clust_res()); clust_res()$data %>% group_by(cluster) %>% summarise(Count = n(), Avg_Risk = percent(mean(churn_probability)), Avg_Vol = dollar(mean(total_tx_volume))) %>% datatable(options = list(dom = 't'), rownames = FALSE) })
  

  dt_model <- eventReactive(input$run_dt, { data <- df_raw %>% select(all_of(ml_vars)) %>% na.omit(); rpart(churn_probability ~ ., data = data, method = "anova", control = rpart.control(cp = input$dt_cp, maxdepth = input$dt_depth)) }, ignoreNULL = TRUE)
  output$dt_plot <- renderDiagonalNetwork({ req(dt_model()); diagonalNetwork(List = ToListExplicit(as.Node(dt_model()), unname = TRUE), fontSize = 12, opacity = 0.9) })
  output$dt_cp_plot <- renderPlot({ req(dt_model()); plotcp(dt_model()) })
  rf_model <- eventReactive(input$run_rf, { data <- df_raw %>% select(all_of(ml_vars)) %>% na.omit(); set.seed(1234); trainIdx <- createDataPartition(data$churn_probability, p = 0.8, list = FALSE); fit <- ranger(churn_probability ~ ., data = data[trainIdx, ], num.trees = input$rf_trees, importance = "impurity"); test <- data[-trainIdx, ]; test$pred <- predict(fit, test)$predictions; list(model = fit, test = test, data = data) }, ignoreNULL = TRUE)
  output$rf_r2_val <- renderUI({ req(rf_model()); paste0(round(rf_model()$model$r.squared * 100, 2), "%") })
  output$rf_mae_val <- renderUI({ req(rf_model()); paste0(round(mean(abs(rf_model()$test$churn_probability - rf_model()$test$pred)), 4)) })
  output$rf_scatter <- renderPlotly({ req(rf_model()); p <- ggplot(rf_model()$test, aes(x = churn_probability, y = pred)) + geom_point(alpha = 0.4, color = "#C5A381") + geom_abline(slope = 1, intercept = 0, color = "#8D6E63", linetype = "dashed") + theme_minimal(); ggplotly(p) })
  output$rf_imp <- renderPlot({ req(rf_model()); imp <- as.data.frame(rf_model()$model$variable.importance) %>% rename(Importance = 1) %>% mutate(Variable = rownames(.)); ggplot(imp, aes(x = reorder(Variable, Importance), y = Importance)) + geom_bar(stat = "identity", fill = "#E3D5CA") + coord_flip() + theme_minimal() })
  sim_prob <- eventReactive(input$predict_sim, { req(rf_model()); new_data <- data.frame(age = input$sim_age, occupation = factor(input$sim_occ, levels = levels(rf_model()$data$occupation)), income_bracket = factor("Medium", levels = levels(rf_model()$data$income_bracket)), active_products = input$sim_active, tx_count = 50, customer_tenure = 24, failed_transactions = 0, credit_utilization_ratio = 0.3, satisfaction_score = input$sim_sat, complaint_topics = factor("No Complaints", levels = levels(rf_model()$data$complaint_topics))); round(predict(rf_model()$model, new_data)$predictions * 100, 2) })
  output$sim_result_ui <- renderUI({ req(sim_prob()); prob <- sim_prob(); value_box(title = "Estimated Risk", value = paste0(prob, "%"), showcase = bs_icon(if(prob > 70) "exclamation-triangle" else "check-circle"), theme = value_box_theme(bg = if(prob > 70) "#8D6E63" else "#C5A381", fg = "white")) })
  conf_res <- eventReactive(input$run_conf, { 
    req(input$conf_gender, input$conf_location)
    res <- df_raw %>% 
      filter(gender %in% input$conf_gender, location %in% input$conf_location)
    if (nrow(res) > 3000) {
      set.seed(123)
      res <- res %>% sample_n(3000)
    }
    
    return(res)
  }, ignoreNULL = TRUE)
  output$conf_plot <- renderPlot({ req(conf_res()); ggbetweenstats(data = conf_res(), x = !!sym(input$conf_x), y = !!sym(input$conf_y), palette = "Set2") })
}

shinyApp(ui, server)