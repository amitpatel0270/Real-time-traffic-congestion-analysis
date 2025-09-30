# app.R - Advanced R Shiny Traffic Dashboard
# -----------------------------
# Place this file and your dataset (traffic_data1.csv) in the same folder.
# Start R or RStudio, set working directory to the folder, then run:
#   install.packages(c("shiny","shinydashboard","tidyverse","lubridate","DT","plotly","scales","heatmaply","caret","randomForest"))
#   shiny::runApp()
# -----------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(DT)
library(plotly)
library(scales)
library(heatmaply)
library(caret)
library(randomForest)

# UI -----------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Traffic Analysis Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("chart-line")),
      menuItem("Location Heatmap", tabName = "heatmap", icon = icon("map")),
      menuItem("Weather & Events", tabName = "weather", icon = icon("cloud-sun-rain")),
      menuItem("Alerts", tabName = "alerts", icon = icon("exclamation-triangle")),
      menuItem("Modeling", tabName = "modeling", icon = icon("cogs")),
      menuItem("Clustering", tabName = "clustering", icon = icon("project-diagram")),
      hr(),
      fileInput("file1", "Upload traffic_data1.csv (optional)", accept = c('.csv')),
      checkboxInput("use_sample", "Use sample bundled dataset if no upload", value = TRUE),
      selectInput("location_select", "Location", choices = NULL, selected = NULL, multiple = TRUE),
      dateRangeInput("date_range", "Date range", start = NULL, end = NULL),
      sliderInput("hour_range", "Hour of day", min = 0, max = 23, value = c(0,23)),
      actionButton("apply_filters", "Apply Filters", icon = icon("filter")),
      width = 300
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.small-box {height: 110px}'))),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_records", width = 3),
                valueBoxOutput("avg_congestion", width = 3),
                valueBoxOutput("alerts_count", width = 3),
                valueBoxOutput("unique_locations", width = 3)
              ),
              fluidRow(
                box(title = "Daily Total Vehicles", status = "primary", solidHeader = TRUE, width = 8, plotlyOutput("daily_ts")),
                box(title = "Top Locations (avg vehicles)", status = "info", solidHeader = TRUE, width = 4, plotlyOutput("top_locations"))
              )
      ),
      tabItem(tabName = "timeseries",
              fluidRow(
                box(width = 12, title = "Time Series Explorer", solidHeader = TRUE,
                    selectInput("ts_location", "Choose location", choices = NULL),
                    checkboxInput("ts_decompose", "Show seasonal decomposition (weekly)", value = TRUE),
                    plotlyOutput("ts_plot"),
                    uiOutput("decomp_ui")
                )
              )
      ),
      tabItem(tabName = "heatmap",
              fluidRow(
                box(width = 12, title = "Hourly Heatmap (Location vs Hour)", solidHeader = TRUE,
                    plotlyOutput("heatmap_plot"))
              )
      ),
      tabItem(tabName = "weather",
              fluidRow(
                box(width = 6, title = "Mean Congestion by Weather", solidHeader = TRUE, plotlyOutput("weather_bar")),
                box(width = 6, title = "Rainfall vs Congestion", solidHeader = TRUE, plotlyOutput("rain_box"))
              ),
              fluidRow(
                box(width = 12, title = "Event Impact (Event vs Non-Event)", solidHeader = TRUE, plotlyOutput("event_box"))
              )
      ),
      tabItem(tabName = "alerts",
              fluidRow(
                box(width = 12, title = "Alerts (congestion_index >= 95th percentile)", solidHeader = TRUE,
                    DTOutput("alerts_table"),
                    downloadButton("download_alerts", "Download Alerts CSV")
                )
              )
      ),
      tabItem(tabName = "modeling",
              fluidRow(
                box(width = 4, title = "Model Controls", solidHeader = TRUE,
                    radioButtons("model_choice", "Model", choices = c("RandomForest" = "rf", "GradientBoosting" = "gbm"), selected = "rf"),
                    numericInput("train_frac", "Train fraction (time-based)", value = 0.8, min = 0.5, max = 0.95, step = 0.05),
                    actionButton("train_model", "Train Model", icon = icon("play"))
                ),
                box(width = 8, title = "Model Performance", solidHeader = TRUE,
                    verbatimTextOutput("model_metrics"),
                    plotlyOutput("feature_importance")
                )
              ),
              fluidRow(
                box(width = 12, title = "Predictions (recent)", solidHeader = TRUE, DTOutput("predictions_table"))
              )
      ),
      tabItem(tabName = "clustering",
              fluidRow(
                box(width = 4, title = "Clustering Controls", solidHeader = TRUE,
                    numericInput("k_clusters", "Number of clusters", value = 3, min = 2, max = 10),
                    actionButton("run_cluster", "Run Clustering", icon = icon("cubes"))
                ),
                box(width = 8, title = "Cluster Centroids (Hourly Profiles)", solidHeader = TRUE, plotlyOutput("cluster_centroids"))
              ),
              fluidRow(
                box(width = 12, title = "Location Cluster Assignments", solidHeader = TRUE, DTOutput("cluster_table"), downloadButton("download_clusters","Download Clusters"))
              )
      )
    )
  )
)

# Server --------------------------------------------------------------
server <- function(input, output, session) {

  # Reactive: load dataset (uploaded or default)
  data_raw <- reactive({
    req(input$use_sample || !is.null(input$file1))
    if (!is.null(input$file1)) {
      df <- read_csv(input$file1$datapath, show_col_types = FALSE)
    } else {
      # assume traffic_data1.csv in app directory
      df <- read_csv("traffic_data1.csv", show_col_types = FALSE)
    }
    # basic parsing
    df <- df %>% mutate(timestamp = dmy_hm(timestamp),
                        date = as_date(timestamp),
                        hour_of_day = hour(timestamp),
                        day_of_week = wday(timestamp, label = TRUE, abbr = FALSE),
                        is_weekend = if_else(day_of_week %in% c('Saturday','Sunday'), 1, 0))
    return(df)
  })

  # Observe file and update filters
  observeEvent(data_raw(), {
    df <- data_raw()
    locs <- unique(df$location)
    updateSelectInput(session, "location_select", choices = locs, selected = head(locs, 5))
    updateSelectInput(session, "ts_location", choices = locs, selected = locs[1])
    rng <- range(df$date)
    updateDateRangeInput(session, "date_range", start = rng[1], end = rng[2])
  })

  # Apply filters reactive
  filtered <- eventReactive(input$apply_filters, {
    df <- data_raw()
    # location filter
    if (!is.null(input$location_select) && length(input$location_select)>0) df <- df %>% filter(location %in% input$location_select)
    # date range
    if (!is.null(input$date_range)) df <- df %>% filter(date >= input$date_range[1] & date <= input$date_range[2])
    # hour range
    df <- df %>% filter(hour_of_day >= input$hour_range[1] & hour_of_day <= input$hour_range[2])
    return(df)
  }, ignoreNULL = FALSE)

  # Overview value boxes
  output$total_records <- renderValueBox({
    v <- nrow(filtered())
    valueBox(formatC(v, format = 'd', big.mark = ','), "Records", icon = icon("table"), color = "purple")
  })
  output$avg_congestion <- renderValueBox({
    df <- filtered()
    avgc <- round(mean(df$congestion_index, na.rm = TRUE),2)
    valueBox(avgc, "Avg congestion_index", icon = icon("chart-area"), color = "yellow")
  })
  output$alerts_count <- renderValueBox({
    df <- filtered()
    th <- quantile(df$congestion_index, 0.95, na.rm = TRUE)
    ac <- sum(df$congestion_index >= th, na.rm = TRUE)
    valueBox(ac, "Alerts (95th pct)", icon = icon("exclamation-triangle"), color = "red")
  })
  output$unique_locations <- renderValueBox({
    valueBox(length(unique(filtered()$location)), "Locations", icon = icon("map-marker-alt"), color = "blue")
  })

  # Daily time series
  output$daily_ts <- renderPlotly({
    df <- filtered()
    daily <- df %>% group_by(date) %>% summarise(total = sum(vehicle_count, na.rm = TRUE)) %>% ungroup()
    p <- ggplot(daily, aes(x = date, y = total)) + geom_line() + labs(x = "Date", y = "Total vehicles") + theme_minimal()
    ggplotly(p)
  })

  # Top locations
  output$top_locations <- renderPlotly({
    df <- filtered()
    topn <- df %>% group_by(location) %>% summarise(avg_v = mean(vehicle_count, na.rm = TRUE)) %>% arrange(desc(avg_v)) %>% head(10)
    p <- ggplot(topn, aes(x = reorder(location, avg_v), y = avg_v)) + geom_col() + coord_flip() + labs(x = "Location", y = "Avg vehicles") + theme_minimal()
    ggplotly(p)
  })

  # Time series per location + decomposition
  output$ts_plot <- renderPlotly({
    req(input$ts_location)
    df <- data_raw() %>% filter(location == input$ts_location)
    daily <- df %>% group_by(date) %>% summarise(total = sum(vehicle_count, na.rm = TRUE)) %>% ungroup()
    p <- ggplot(daily, aes(x = date, y = total)) + geom_line() + labs(title = paste("Daily vehicles -", input$ts_location), x = "Date", y = "Total vehicles") + theme_minimal()
    ggplotly(p)
  })

  output$decomp_ui <- renderUI({
    if (input$ts_decompose) {
      plotOutput("decomp_plots")
    } else NULL
  })

  output$decomp_plots <- renderPlot({
    req(input$ts_location)
    df <- data_raw() %>% filter(location == input$ts_location)
    daily <- df %>% group_by(date) %>% summarise(total = sum(vehicle_count, na.rm = TRUE)) %>% arrange(date)
    # need at least 14 points for weekly decompose
    if (nrow(daily) < 14) {
      plot.new(); text(0.5,0.5, "Not enough data for decomposition (need >= 14 days)")
      return()
    }
    ts_obj <- ts(daily$total, frequency = 7)
    decomp <- stats::decompose(ts_obj)
    plot(decomp)
  })

  # Heatmap: location vs hour
  output$heatmap_plot <- renderPlotly({
    df <- filtered()
    prof <- df %>% group_by(location, hour_of_day) %>% summarise(avg = mean(vehicle_count, na.rm = TRUE)) %>% ungroup()
    pivot <- prof %>% pivot_wider(names_from = hour_of_day, values_from = avg, values_fill = 0)
    mat <- as.matrix(pivot[,-1])
    rownames(mat) <- pivot$location
    # use heatmaply for interactive heatmap
    heatmaply::heatmaply(mat, xlab = "Hour of Day", ylab = "Location", scale = "row", hide_colorbar = FALSE)
  })

  # Weather & Rain
  output$weather_bar <- renderPlotly({
    df <- filtered()
    tab <- df %>% group_by(weather) %>% summarise(mean_cong = mean(congestion_index, na.rm = TRUE), n = n()) %>% arrange(desc(mean_cong))
    p <- ggplot(tab, aes(x = reorder(weather, -mean_cong), y = mean_cong, text = paste("n=",n))) + geom_col() + labs(x = "Weather", y = "Mean congestion_index") + theme_minimal()
    ggplotly(p, tooltip = c("x","y","text"))
  })

  output$rain_box <- renderPlotly({
    df <- filtered()
    df <- df %>% mutate(rain_bin = cut(rainfall_mm, breaks = c(-0.01,0,2,10,10000), labels = c('NoRain','Light','Moderate','Heavy')))
    p <- ggplot(df, aes(x = rain_bin, y = congestion_index)) + geom_boxplot() + labs(x = "Rain bin", y = "congestion_index") + theme_minimal()
    ggplotly(p)
  })

  output$event_box <- renderPlotly({
    df <- filtered()
    df <- df %>% mutate(is_event = if_else(is.na(event) | event=="" | tolower(event) %in% c('none','na'), FALSE, TRUE))
    tab <- df %>% group_by(is_event) %>% summarise(mean_cong = mean(congestion_index, na.rm = TRUE), n = n())
    p <- ggplot(df, aes(x = is_event, y = congestion_index)) + geom_boxplot() + labs(x = "Event", y = "congestion_index") + theme_minimal()
    ggplotly(p)
  })

  # Alerts table
  alerts_df <- reactive({
    df <- filtered()
    th <- quantile(df$congestion_index, 0.95, na.rm = TRUE)
    alerts <- df %>% filter(congestion_index >= th) %>% arrange(desc(congestion_index))
    alerts
  })
  output$alerts_table <- renderDT({
    datatable(alerts_df(), options = list(pageLength = 10, scrollX = TRUE))
  })
  output$download_alerts <- downloadHandler(
    filename = function() { paste0('traffic_alerts_', Sys.Date(), '.csv') },
    content = function(file) { write_csv(alerts_df(), file) }
  )

  # Modeling
  model_storage <- reactiveValues(rf = NULL, gbm = NULL, last_metrics = NULL)

  observeEvent(input$train_model, {
    df <- data_raw()
    # features and simple preprocessing
    df2 <- df %>% mutate(event_flag = if_else(is.na(event) | event=="" | tolower(event) %in% c('none','na'), 0, 1))
    # encode top locations
    top_locs <- df2 %>% count(location) %>% top_n(10, n) %>% pull(location)
    df2 <- df2 %>% mutate(location_enc = if_else(location %in% top_locs, location, 'Other'))
    # prepare X,y
    feats <- c('hour_of_day','is_weekend','rainfall_mm','vehicle_count','heavy_vehicle_count','light_vehicle_count','avg_speed','traffic_density','event_flag')
    X <- df2 %>% select(all_of(feats)) %>% mutate(across(everything(), ~replace_na(.,0)))
    X <- cbind(X, model.matrix(~location_enc -1, df2))
    y <- df2$congestion_index
    # time-based split
    ntrain <- floor(nrow(df2)*input$train_frac)
    train_idx <- 1:ntrain
    test_idx <- (ntrain+1):nrow(df2)
    trX <- X[train_idx,]
    trY <- y[train_idx]
    teX <- X[test_idx,]
    teY <- y[test_idx]
    # RandomForest
    if (input$model_choice == 'rf'){
      fit <- randomForest(x = trX, y = trY, ntree = 200)
      preds <- predict(fit, teX)
      model_storage$rf <- fit
    } else {
      fit <- train(x = trX, y = trY, method = 'gbm', verbose = FALSE, trControl = trainControl(method = 'none'))
      preds <- predict(fit, teX)
      model_storage$gbm <- fit
    }
    metrics <- list(MAE = mae <- mean(abs(teY - preds)), RMSE = sqrt(mean((teY - preds)^2)), R2 = cor(teY, preds)^2)
    model_storage$last_metrics <- metrics
    # save predictions to reactive
    model_storage$preds <- tibble(timestamp = df2$timestamp[test_idx], location = df2$location[test_idx], actual = teY, pred = preds)
  })

  output$model_metrics <- renderPrint({
    req(model_storage$last_metrics)
    print(model_storage$last_metrics)
  })

  output$feature_importance <- renderPlotly({
    req(model_storage$rf)
    imp <- importance(model_storage$rf)
    imp_df <- data.frame(feature = rownames(imp), importance = imp[,1]) %>% arrange(desc(importance)) %>% head(15)
    p <- ggplot(imp_df, aes(x = reorder(feature, importance), y = importance)) + geom_col() + coord_flip() + theme_minimal() + labs(x = '', y = 'Importance')
    ggplotly(p)
  })

  output$predictions_table <- renderDT({
    req(model_storage$preds)
    datatable(model_storage$preds %>% arrange(desc(timestamp)), options = list(pageLength = 10, scrollX = TRUE))
  })

  # Clustering
  observeEvent(input$run_cluster, {
    df <- data_raw()
    prof <- df %>% group_by(location, hour_of_day) %>% summarise(avg = mean(vehicle_count, na.rm = TRUE)) %>% ungroup()
    pivot <- prof %>% pivot_wider(names_from = hour_of_day, values_from = avg, values_fill = 0)
    locs <- pivot$location
    mat <- as.matrix(pivot %>% select(-location))
    mat_s <- scale(mat)
    km <- kmeans(mat_s, centers = input$k_clusters, nstart = 25)
    pivot$cluster <- factor(km$cluster)
    model_storage$clusters <- pivot
    model_storage$centroids <- km$centers
  })

  output$cluster_centroids <- renderPlotly({
    req(model_storage$centroids)
    cents <- model_storage$centroids
    dfc <- as_tibble(cents)
    dfc$cluster <- paste0('C', 1:nrow(dfc))
    dfm <- dfc %>% pivot_longer(-cluster, names_to = 'hour', values_to = 'value') %>% mutate(hour = as.integer(gsub('V','',hour))-1)
    p <- ggplot(dfm, aes(x = hour, y = value, color = cluster)) + geom_line() + theme_minimal() + labs(x = 'Hour of day', y = 'Avg vehicles')
    ggplotly(p)
  })

  output$cluster_table <- renderDT({
    req(model_storage$clusters)
    datatable(model_storage$clusters %>% select(location, cluster), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$download_clusters <- downloadHandler(
    filename = function() { paste0('location_clusters_', Sys.Date(), '.csv') },
    content = function(file) { write_csv(model_storage$clusters %>% select(location, cluster), file) }
  )

}

# Run the app ---------------------------------------------------------
shinyApp(ui, server)
