
pacman::p_load(
  shiny, bslib, tidyverse, corrplot, plotly, DT, scales, 
  ggstatsplot, shinyWidgets, rpart, rpart.plot, networkD3, 
  data.tree, shinydashboard, shinycustomloader, bsicons, caret, ranger
)

# ---------------------------------------------------------------- 1. Global Data
# 讀取全量數據
df_raw <- read_csv("colombia_customers_processed.csv", show_col_types = FALSE) %>%
  mutate(across(where(is.character), as.factor))

ml_vars <- c("churn_probability", "age", "occupation", "income_bracket", 
             "active_products", "tx_count", "customer_tenure", "failed_transactions", 
             "credit_utilization_ratio", "satisfaction_score", "complaint_topics")

# ==============================================================================
# UI (使用者介面)
# ==============================================================================

ui <- page_navbar(
  title = "More Than Just Beans",
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#C5A381", secondary = "#8D6E63"),
  
  header = tags$head(
    tags$style(HTML("
      .navbar { background-color: #C5A381 !important; }
      .btn-milktea { background-color: #C5A381 !important; border-color: #B28F6F !important; color: white !important; font-weight: 600 !important; transition: all 0.3s ease; }
      .btn-milktea:hover { background-color: #A67B5B !important; color: #f8f9fa !important; box-shadow: 0 4px 8px rgba(0,0,0,0.15); }
      .card-header { background-color: #E3D5CA !important; color: #5D4037 !important; font-weight: bold; }
      .sim-header { color: #8D6E63; border-left: 5px solid #C5A381; padding-left: 15px; margin-bottom: 20px; font-weight: bold; }
      .active-strat-card { border-radius: 15px; padding: 25px; text-align: center; border: 3px solid #C5A381; background: #ffffff; box-shadow: 0 6px 20px rgba(0,0,0,0.15); }
    "))
  ),
  
  # --- EDA Explorer ---
  nav_panel("📊 EDA Explorer",
            layout_columns(
              value_box("Total Customers", nrow(df_raw), showcase = bs_icon("people"), theme = "secondary"),
              value_box("Avg Churn Risk", percent(mean(df_raw$churn_probability)), showcase = bs_icon("graph-down"), theme = "danger"),
              value_box("Avg Satisfaction", round(mean(df_raw$satisfaction_score), 2), showcase = bs_icon("emoji-smile"), theme = "info")
            ),
            sidebarLayout(
              sidebarPanel(
                selectInput("dist_var", "Select Variable:", choices = c("churn_probability", "customer_tenure", "satisfaction_score", "active_products")),
                selectInput("dist_segment", "Filter by CLV Segment:", choices = c("All", sort(unique(as.character(df_raw$clv_segment)))))
              ),
              mainPanel(withLoader(plotlyOutput("distPlot", height = "500px"), type="html", loader="loader4"))
            )
  ),
  
  # --- Group Analysis (CDA) ---
  nav_panel("⚖️ Group Analysis",
            sidebarLayout(
              sidebarPanel(
                checkboxGroupInput("conf_gender", "Select Gender:", choices = c("Male", "Female", "Other"), selected = c("Male", "Female", "Other")),
                pickerInput("conf_location", "Location:", choices = sort(unique(as.character(df_raw$location))), 
                            multiple = TRUE, options = list(`actions-box` = TRUE), selected = unique(as.character(df_raw$location))[1:3]),
                hr(),
                actionButton("run_conf", "Run Statistical Test", class = "btn-milktea w-100")
              ),
              mainPanel(
                layout_columns(
                  card(card_header("Statistical Visualization"), 
                       withLoader(plotOutput("conf_plot", height = "500px"), type="html", loader="loader4")),
                  card(card_header("Test Configuration"), 
                       selectInput("conf_x", "Categorical Variable (X):", choices = c("income_bracket", "customer_segment")),
                       selectInput("conf_y", "Numerical Variable (Y):", choices = c("churn_probability", "satisfaction_score")))
                )
              )
            )
  ),
  
  # --- Strategic Clustering ---
  nav_panel("🎯 Strategic Clustering",
            sidebarLayout(
              sidebarPanel(
                sliderInput("clust_k", "Clusters (k):", 2, 8, 3),
                h5("Strategic Weights"),
                sliderInput("w_vol", "Volume Weight:", 0, 1, 0.6, step = 0.1),
                sliderInput("w_risk", "Risk Weight:", 0, 1, 0.3, step = 0.1),
                actionButton("run_clust", "Execute Analysis", class = "btn-milktea w-100", icon = icon("sync")),
                br(),
                span("Note: Clustering uses a random sample of 5,000 records for performance.", 
                     style = "font-size: 0.8rem; color: gray;")
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
                  card(card_header("Segment Personas"), DTOutput("persona_table"))
                )
              )
            )
  ),
  
  # --- Decision Tree ---
  nav_panel("🌲 Decision Tree",
            sidebarLayout(
              sidebarPanel(
                h5("🌲 Tree Parameters"),
                sliderInput("dt_cp", "Complexity (CP):", 0.0001, 0.05, 0.001, step = 0.0005),
                sliderInput("dt_depth", "Max Depth:", 1, 15, 7),
                hr(),
                h5("👥 Filter Population"),
                selectInput("dt_filter_income", "Income Bracket:", choices = c("All", "Low", "Medium", "High")),
                hr(),
                actionButton("run_dt", "Generate Tree", icon = icon("bolt"), class = "btn-milktea w-100")
              ),
              mainPanel(
                layout_column_wrap(
                  width = 1/3,
                  value_box("Subgroup Size", textOutput("dt_obs_count"), showcase = bs_icon("people-fill"), theme = "secondary"),
                  value_box("Average Risk", textOutput("dt_avg_risk"), showcase = bs_icon("exclamation-triangle-fill"), theme = "danger"),
                  value_box("Top Occupation", textOutput("dt_top_occ"), showcase = bs_icon("briefcase-fill"), theme = "info")
                ),
                br(),
                layout_column_wrap(
                  width = 1/2,
                  card(
                    card_header("Interactive Decision Path (D3)"),
                    withLoader(diagonalNetworkOutput("dt_plot", height = "500px"), type="html", loader="loader4"),
                    full_screen = TRUE
                  ),
                  card(
                    card_header("Pruning Diagnostic (CP Plot)"),
                    plotOutput("dt_cp_plot", height = "500px"),
                    card_footer("Note: CP plot helps to find the optimal tree size.")
                  )
                )
              )
            )
  ),
  
  # --- Random Forest ---
  nav_panel("🦊 Random Forest",
            sidebarLayout(
              sidebarPanel(
                h5("⚙️ Model Settings"),
                sliderInput("rf_trees", "Number of Trees:", 10, 200, 50),
                hr(),
                actionButton("run_rf", "Train Model", icon = icon("bolt"), class = "btn-milktea w-100")
              ),
              mainPanel(
                layout_column_wrap(
                  width = 1/2,
                  card(card_header("Actual vs Predicted Results"), plotlyOutput("rf_scatter", height = "400px")),
                  card(card_header("Variable Importance (Drivers)"), plotOutput("rf_imp", height = "400px"))
                ),
                hr(),
                h3("🔍 Churn Simulator", class = "sim-header"),
                card(
                  card_body(
                    layout_column_wrap(
                      width = 1/2,
                      div(
                        h5("Step 1: Input Features"),
                        layout_column_wrap(
                          width = 1/2,
                          numericInput("sim_age", "Age:", value = 35),
                          selectInput("sim_occ", "Occupation:", choices = sort(unique(as.character(df_raw$occupation)))),
                          numericInput("sim_active", "Active Products:", value = 2),
                          numericInput("sim_tenure", "Tenure (Months):", value = 12),
                          sliderInput("sim_sat", "Satisfaction (1-6):", min = 1, max = 6, value = 3)
                        ),
                        actionButton("predict_sim", "Predict Risk Now", icon = icon("calculator"), class = "btn-milktea w-100")
                      ),
                      div(
                        h5("Step 2: Prediction Result"),
                        uiOutput("sim_result_ui"),
                        value_box(title = "Model Confidence (R²)", value = textOutput("rf_r2_val"), showcase = bs_icon("shield-check"), theme = "secondary")
                      )
                    )
                  )
                )
              )
            )
  )
)

# ==============================================================================
# SERVER (伺服器邏輯)
# ==============================================================================

server <- function(input, output, session) {
  
  # --- EDA ---
  output$distPlot <- renderPlotly({
    df <- if(input$dist_segment == "All") df_raw else df_raw %>% filter(clv_segment == input$dist_segment)
    p <- ggplot(df, aes(x = .data[[input$dist_var]])) + geom_histogram(fill = "#C5A381", color = "white") + theme_minimal()
    ggplotly(p)
  })
  
  # --- Group Analysis (CDA) --- 使用全量數據 + 正常顏色
  conf_res <- eventReactive(input$run_conf, {
    req(input$conf_gender, input$conf_location)
    df_raw %>% filter(gender %in% input$conf_gender, location %in% input$conf_location)
  }, ignoreNULL = TRUE)
  
  output$conf_plot <- renderPlot({
    req(conf_res())
    ggbetweenstats(data = conf_res(), x = !!sym(input$conf_x), y = !!sym(input$conf_y),
                   title = "Group Comparison Analysis") # 使用預設配色
  })
  
  # --- Clustering --- 抽樣 5,000 筆跑
  clust_res <- eventReactive(input$run_clust, {
    set.seed(42)
    # 執行抽樣
    df_s <- df_raw %>% sample_n(min(nrow(.), 5000))
    
    df_c <- df_s %>% mutate(v = scale(total_transaction_volume), r = scale(churn_probability)) %>% select(v, r)
    km <- kmeans(df_c, centers = input$clust_k)
    list(data = df_s %>% mutate(cluster = as.factor(km$cluster)))
  }, ignoreNULL = TRUE)
  
  output$priority_boxes <- renderUI({
    req(clust_res())
    d <- clust_res()$data %>% group_by(occupation) %>%
      summarise(count = n(), avg_vol = mean(total_transaction_volume, na.rm = TRUE), 
                avg_sat = mean(satisfaction_score, na.rm = TRUE), 
                avg_churn = mean(churn_probability, na.rm = TRUE)) %>%
      filter(count > 10) %>%
      mutate(score = as.numeric((input$w_vol * scale(log10(avg_vol + 1))) + (0.2 * scale(avg_sat)) - (input$w_risk * scale(avg_churn)))) %>%
      arrange(desc(score)) %>% head(3)
    
    labels <- c("🥇 Rank 1", "🥈 Rank 2", "🥉 Rank 3")
    colors <- c("yellow", "teal", "orange")
    fluidRow(lapply(seq_len(nrow(d)), function(i) {
      column(width = 4, infoBox(labels[i], d$occupation[i], subtitle = paste("Score:", round(d$score[i], 2)), icon = icon("star"), color = colors[i], fill = TRUE, width = NULL))
    }))
  })
  
  output$active_strategy_card <- renderUI({
    v_h <- input$w_vol > 0.5; r_h <- input$w_risk > 0.5
    mode_info <- if (v_h && r_h) list(m="Premium", i="gem") else if (v_h) list(m="Aggressive", i="rocket") else list(m="Defensive", i="shield")
    div(class = "active-strat-card", icon(mode_info$i, style="font-size: 50px; color: #C5A381;"), h3(mode_info$m), p("Active Strategic Focus"))
  })
  
  output$cluster_scatter <- renderPlotly({
    req(clust_res())
    p <- ggplot(clust_res()$data, aes(x = total_transaction_volume, y = churn_probability, color = cluster)) + 
      geom_point(alpha = 0.5) + scale_x_log10(labels = comma) + theme_minimal() + scale_color_brewer(palette = "BrBG")
    ggplotly(p)
  })
  
  output$persona_table <- renderDT({
    req(clust_res())
    clust_res()$data %>% group_by(cluster) %>% 
      summarise(Count = n(), Avg_Risk = percent(mean(churn_probability)), Avg_Vol = dollar(mean(total_transaction_volume))) %>% 
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
  # --- Decision Tree --- 使用全量數據
  dt_model_data <- eventReactive(input$run_dt, {
    data <- df_raw %>% select(all_of(ml_vars)) %>% na.omit()
    if (input$dt_filter_income != "All") data <- data %>% filter(income_bracket == input$dt_filter_income)
    fit <- rpart(churn_probability ~ ., data = data, method = "anova", 
                 control = rpart.control(cp = input$dt_cp, maxdepth = input$dt_depth))
    list(model = fit, df = data)
  }, ignoreNULL = TRUE)
  
  output$dt_obs_count <- renderText({ req(dt_model_data()); nrow(dt_model_data()$df) })
  output$dt_avg_risk <- renderText({ req(dt_model_data()); paste0(round(mean(dt_model_data()$df$churn_probability) * 100, 1), "%") })
  output$dt_top_occ <- renderText({ 
    req(dt_model_data())
    occ <- dt_model_data()$df %>% count(occupation) %>% arrange(desc(n))
    if(nrow(occ) > 0) as.character(occ$occupation[1]) else "N/A" 
  })
  
  output$dt_plot <- renderDiagonalNetwork({
    req(dt_model_data())
    tree_list <- ToListExplicit(as.Node(dt_model_data()$model), unname = TRUE)
    diagonalNetwork(List = tree_list, fontSize = 12, opacity = 0.9)
  })
  
  output$dt_cp_plot <- renderPlot({ req(dt_model_data()); plotcp(dt_model_data()$model) })
  
  # --- Random Forest --- 使用全量數據
  rf_model_run <- eventReactive(input$run_rf, {
    data <- df_raw %>% select(all_of(ml_vars)) %>% na.omit()
    set.seed(1234)
    trainIdx <- createDataPartition(data$churn_probability, p = 0.8, list = FALSE)
    fit <- ranger(churn_probability ~ ., data = data[trainIdx, ], num.trees = input$rf_trees, importance = "impurity")
    test_data <- data[-trainIdx, ]
    test_data$pred <- predict(fit, test_data)$predictions
    list(model = fit, test_data = test_data, data = data)
  }, ignoreNULL = TRUE)
  
  output$rf_scatter <- renderPlotly({
    req(rf_model_run())
    p <- ggplot(rf_model_run()$test_data, aes(x = churn_probability, y = pred)) + 
      geom_point(alpha = 0.4, color = "#C5A381") + geom_abline(slope = 1, intercept = 0, color = "#8D6E63", linetype = "dashed") + theme_minimal()
    ggplotly(p)
  })
  
  output$rf_imp <- renderPlot({
    req(rf_model_run())
    imp <- as.data.frame(rf_model_run()$model$variable.importance) %>% rename(Importance = 1) %>% mutate(Variable = rownames(.))
    ggplot(imp, aes(x = reorder(Variable, Importance), y = Importance)) + geom_bar(stat = "identity", fill = "#E3D5CA") + coord_flip() + theme_minimal()
  })
  
  output$rf_r2_val <- renderText({
    req(rf_model_run())
    paste0(round(cor(rf_model_run()$test_data$churn_probability, rf_model_run()$test_data$pred)^2 * 100, 1), "%")
  })
  
  sim_prediction <- eventReactive(input$predict_sim, {
    req(rf_model_run())
    new_data <- data.frame(
      age = input$sim_age, occupation = factor(input$sim_occ, levels = levels(rf_model_run()$data$occupation)),
      income_bracket = factor("Medium", levels = levels(rf_model_run()$data$income_bracket)),
      active_products = input$sim_active, tx_count = 50, customer_tenure = input$sim_tenure,
      failed_transactions = 0, credit_utilization_ratio = 0.3, satisfaction_score = input$sim_sat,
      complaint_topics = factor("No Complaints", levels = levels(rf_model_run()$data$complaint_topics))
    )
    round(predict(rf_model_run()$model, new_data)$predictions * 100, 2)
  })
  
  output$sim_result_ui <- renderUI({
    req(sim_prediction())
    prob <- sim_prediction()
    value_box(title = "Estimated Risk Probability", value = paste0(prob, "%"),
              showcase = bs_icon(if(prob > 70) "exclamation-triangle" else "check-circle"),
              theme = value_box_theme(bg = if(prob > 70) "#8D6E63" else "#C5A381", fg = "white"))
  })
}

shinyApp(ui, server)