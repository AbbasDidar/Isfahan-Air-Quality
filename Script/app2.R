library(shiny)
library(bslib)
library(dplyr)
library(DT)

# Simulate data
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

ui <- page_sidebar(
  title = "Airport Weather Data Explorer",
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
                  filter = 'top')
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
}

shinyApp(ui, server)
