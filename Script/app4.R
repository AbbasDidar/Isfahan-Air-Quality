library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(ggplot2)
library(tidyr)
library(scales)
library(tseries)
library(forecast)
library(randomForest)

# Simulate data with PM2.5
set.seed(123)
n_rows <- 1000

dates <- seq(as.Date("2020-01-01"), as.Date("2025-12-31"), by="day")
wind_directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
cloud_conditions <- c("Clear", "Scattered", "Broken", "Overcast", NA)

simulated_data <- data.frame(
  Date = sample(dates, n_rows, replace = TRUE),
  WindSpeed = round(rweibull(n_rows, shape = 2, scale = 3), 1),
  WindDirection = sample(wind_directions, n_rows, replace = TRUE),
  WindAngel = sample(0:360, n_rows, replace = TRUE),
  Temperature = round(rnorm(n_rows, mean = 16.3, sd = 11.6), 1),
  RelativityHumidity = round(pmin(pmax(rnorm(n_rows, mean = 35.6, sd = 24.9), 0), 100)),
  Shabnam_temp = round(rnorm(n_rows, mean = -2.83, sd = 5.81), 1),
  Pressure = round(rnorm(n_rows, mean = 1019, sd = 4.85), 1),
  Vision = round(sample(c(NA, rnorm(n_rows, mean = 8.79, sd = 2.25)), n_rows, 
                        prob = c(0.032, rep(0.968/n_rows, n_rows))), 1),
  Cloud = sample(cloud_conditions, n_rows, replace = TRUE, 
                 prob = c(0.2, 0.2, 0.2, 0.05, 0.35))
)

# Add PM2.5 (influenced by other variables plus some random noise)
simulated_data$PM2.5 <- with(simulated_data,
                             35 + 
                               2 * WindSpeed + 
                               0.5 * Temperature + 
                               0.2 * RelativityHumidity +
                               rnorm(n_rows, mean = 0, sd = 10))

# Sort by date for time series analysis
simulated_data <- simulated_data |> mutate(PM2.5 = round(PM2.5, 2)) |> arrange(Date)

# Update UI to include the new Forecast tab
ui <- navset_card_tab(
  # [Previous Data Explorer tab code remains the same]
  nav_panel(
    title = "Data Explorer",
    page_sidebar(
      theme = bs_theme(
        bootswatch = "darkly",
        bg = "#222222",
        fg = "#86C7ED",
        success = "#86C7ED"
      ),
      
      title = "Dark Theme Demo",
      sidebar = sidebar(
        dateRangeInput("date_range", "Date Range",
                       start = min(simulated_data$Date),
                       end = max(simulated_data$Date)),
        sliderInput("wind_speed", "Wind Speed (m/s)",
                    min = 0, max = max(simulated_data$WindSpeed),
                    value = c(0, max(simulated_data$WindSpeed))),
        selectInput("wind_direction", "Wind Direction",
                    choices = c("All", unique(simulated_data$WindDirection))),
        sliderInput("temperature", "Temperature (°C)",
                    min = min(simulated_data$Temperature),
                    max = max(simulated_data$Temperature),
                    value = c(min(simulated_data$Temperature), 
                              max(simulated_data$Temperature))),
        sliderInput("humidity", "Relative Humidity (%)",
                    min = 0, max = 100,
                    value = c(0, 100)),
        selectInput("cloud", "Cloud Conditions",
                    choices = c("All", unique(na.omit(simulated_data$Cloud)))),
        downloadButton("download", "Download Data")
      ),
      layout_columns(
        value_box(
          title = "Total Records",
          value = textOutput("total_records"),
          showcase = bsicons::bs_icon("table")
        ),
        value_box(
          title = "Total Records",
          value = textOutput("total_records"),
          showcase = bsicons::bs_icon("droplet")
        ),
        value_box(
          title = "Total Records",
          value = textOutput("total_records"),
          showcase = bsicons::bs_icon("speedometer")
        ),
        value_box(
          title = "Total Records",
          value = textOutput("total_records"),
          showcase = bsicons::bs_icon("exclamation-triangle")
        ),
        value_box(
          title = "Average Temperature",
          value = textOutput("avg_temp"),
          showcase = bsicons::bs_icon("thermometer-half")
        ),
        value_box(
          title = "Average Wind Speed",
          value = textOutput("avg_wind"),
          showcase = bsicons::bs_icon("wind")
        )
      ),
      card(
        card_header("Filtered Data"),
        DT::dataTableOutput("table")
      )
    )
  ),
  
  # [Previous Descriptive Statistics tab code remains the same]
  nav_panel(
    title = "Descriptive Statistics",
    layout_columns(
      fill = FALSE,
      card(
        card_header("Numerical Variables Summary"),
        DT::dataTableOutput("num_summary")
      ),
      card(
        card_header("Categorical Variables Summary"),
        DT::dataTableOutput("cat_summary")
      )
    ),
    layout_columns(
      card(
        card_header("Temperature Distribution"),
        plotOutput("temp_dist")
      ),
      card(
        card_header("Wind Direction Distribution"),
        plotOutput("wind_rose")
      )
    ),
    layout_columns(
      card(
        card_header("Temperature vs Humidity"),
        plotOutput("temp_humidity")
      ),
      card(
        card_header("Missing Values"),
        plotOutput("missing_plot")
      )
    )
  ),
  
  # New Forecast tab
  nav_panel(
    title = "Forecast",
    layout_columns(
      card(
        card_header("Time Series Variable"),
        selectInput("forecast_var", "Select Variable",
                    choices = c("Temperature", "WindSpeed", "Pressure", "PM2.5")),
        numericInput("forecast_days", "Forecast Days", value = 7, min = 1, max = 30),
        plotOutput("forecast_plot")
      ),
      card(
        card_header("Forecast Values"),
        DT::dataTableOutput("forecast_table")
      )
    ),
    layout_columns(
      card(
        card_header("PM2.5 Prediction Model"),
        plotOutput("model_performance"),
        DT::dataTableOutput("variable_importance")
      )
    )
  )
)

server <- function(input, output, session) {

  
  filtered_data <- reactive({
    data <- simulated_data
    
    # Apply filters
    data <- data %>%
      filter(
        Date >= input$date_range[1] & Date <= input$date_range[2],
        WindSpeed >= input$wind_speed[1] & WindSpeed <= input$wind_speed[2],
        Temperature >= input$temperature[1] & Temperature <= input$temperature[2],
        RelativityHumidity >= input$humidity[1] & RelativityHumidity <= input$humidity[2]
      )
    
    if (input$wind_direction != "All") {
      data <- data %>% filter(WindDirection == input$wind_direction)
    }
    
    if (input$cloud != "All") {
      data <- data %>% filter(Cloud == input$cloud)
    }
    
    data
  })
  
  # Value boxes
  output$total_records <- renderText({
    nrow(filtered_data())
  })
  
  output$avg_temp <- renderText({
    sprintf("%.1f °C", mean(filtered_data()$Temperature))
  })
  
  output$avg_wind <- renderText({
    sprintf("%.1f m/s", mean(filtered_data()$WindSpeed))
  })
  
  # Data table
  output$table <- DT::renderDataTable({
    DT::datatable(filtered_data(),
                  options = list(pageLength = 10,
                                 scrollX = TRUE),
                  filter = 'none')
  })
  
  # Download handler
  output$download <- downloadHandler(
    filename = function() {
      paste("airport_weather_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Numerical summary
  output$num_summary <- DT::renderDataTable({
    num_vars <- simulated_data %>%
      select(where(is.numeric)) %>%
      summarise(across(everything(), 
                       list(
                         Mean = ~mean(., na.rm = TRUE),
                         SD = ~sd(., na.rm = TRUE),
                         Min = ~min(., na.rm = TRUE),
                         Max = ~max(., na.rm = TRUE),
                         Missing = ~sum(is.na(.))
                       ))) %>%
      pivot_longer(everything(), 
                   names_to = c("Variable", "Statistic"), 
                   names_sep = "_") %>%
      pivot_wider(names_from = Statistic, values_from = value) %>%
      mutate(across(where(is.numeric), round, 2))
    
    DT::datatable(num_vars)
  })
  
  # Categorical summary
  output$cat_summary <- DT::renderDataTable({
    cat_summary <- bind_rows(
      simulated_data %>%
        count(WindDirection) %>%
        mutate(Variable = "Wind Direction"),
      simulated_data %>%
        count(Cloud) %>%
        mutate(Variable = "Cloud Conditions")
    ) %>%
      rename(Category = 1, Count = n) %>%
      mutate(Percentage = round(Count / n_rows * 100, 1))
    
    DT::datatable(cat_summary)
  })
  
  # Temperature distribution plot
  output$temp_dist <- renderPlot({
    ggplot(simulated_data, aes(x = Temperature)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      theme_minimal() +
      labs(x = "Temperature (°C)", y = "Count")
  })
  
  # Wind rose plot
  output$wind_rose <- renderPlot({
    wind_summary <- simulated_data %>%
      count(WindDirection) %>%
      mutate(Percentage = n/sum(n) * 100)
    
    ggplot(wind_summary, aes(x = WindDirection, y = Percentage)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_polar() +
      theme_minimal() +
      labs(y = "Percentage (%)")
  })
  
  # Temperature vs Humidity scatter plot
  output$temp_humidity <- renderPlot({
    ggplot(simulated_data, aes(x = Temperature, y = RelativityHumidity)) +
      geom_point(alpha = 0.5, color = "steelblue") +
      theme_minimal() +
      labs(x = "Temperature (°C)", y = "Relative Humidity (%)")
  })
  
  # Missing values plot
  output$missing_plot <- renderPlot({
    missing_summary <- data.frame(
      Variable = names(simulated_data),
      Missing = colSums(is.na(simulated_data))
    ) %>%
      mutate(Percentage = Missing / n_rows * 100)
    
    ggplot(missing_summary, aes(x = reorder(Variable, Percentage), y = Percentage)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(x = "Variable", y = "Missing Values (%)")
  })
  
  
  
  # Forecast functionality
  forecast_data <- reactive({
    var_data <- simulated_data[[input$forecast_var]]
    ts_data <- ts(var_data, frequency = 7)  # Weekly seasonality
    forecast_model <- auto.arima(ts_data)
    forecast(forecast_model, h = input$forecast_days)
  })
  
  output$forecast_plot <- renderPlot({
    fc <- forecast_data()
    autoplot(fc) +
      theme_minimal() +
      labs(title = paste("Forecast for", input$forecast_var),
           y = input$forecast_var,
           x = "Time Period")
  })
  
  output$forecast_table <- renderDT({
    fc <- forecast_data()
    data.frame(
      Day = 1:input$forecast_days,
      Forecast = round(fc$mean, 2),
      Lower_95 = round(fc$lower[,2], 2),
      Upper_95 = round(fc$upper[,2], 2)
    )
  })
  
  # PM2.5 Prediction Model
  model_data <- reactive({
    # Prepare data for modeling
    model_df <- simulated_data %>%
      select(PM2.5, Temperature, WindSpeed, RelativityHumidity, 
             Pressure, WindAngel) %>%
      na.omit()
    
    # Split into training and testing
    train_idx <- sample(1:nrow(model_df), 0.8 * nrow(model_df))
    train_data <- model_df[train_idx, ]
    test_data <- model_df[-train_idx, ]
    
    # Train random forest model
    rf_model <- randomForest(PM2.5 ~ ., data = train_data)
    
    # Make predictions
    predictions <- predict(rf_model, test_data)
    
    list(
      model = rf_model,
      test_actual = test_data$PM2.5,
      test_pred = predictions,
      importance = importance(rf_model)
    )
  })
  
  output$model_performance <- renderPlot({
    mod_data <- model_data()
    plot_data <- data.frame(
      Actual = mod_data$test_actual,
      Predicted = mod_data$test_pred
    )
    
    ggplot(plot_data, aes(x = Actual, y = Predicted)) +
      geom_point(alpha = 0.5) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(title = "PM2.5 Model Performance",
           x = "Actual PM2.5",
           y = "Predicted PM2.5")
  })
  
  output$variable_importance <- renderDT({
    imp_data <- data.frame(
      Variable = rownames(model_data()$importance),
      Importance = model_data()$importance[,1]
    ) %>%
      arrange(desc(Importance))
    
    datatable(imp_data, options = list(pageLength = 5))
  })
  
  # [Previous server code remains unchanged]
}

shinyApp(ui, server)
