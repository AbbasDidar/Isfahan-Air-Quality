library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(lubridate)

# Simulate weather data
set.seed(123)
n_days <- 365
n_hours <- 24
n_rows <- n_days * n_hours

# Create datetime sequence
dates <- seq(as.POSIXct("2023-01-01"), by = "hour", length.out = n_rows)

# Generate weather data
weather_data <- tibble(
  Date = dates,
  WindSpeed = pmax(0, rnorm(n_rows, mean = 10, sd = 5)),
  WindAngle = runif(n_rows, 0, 360),
  WindDirection = case_when(
    WindAngle >= 315 | WindAngle < 45 ~ "N",
    WindAngle >= 45 & WindAngle < 135 ~ "E",
    WindAngle >= 135 & WindAngle < 225 ~ "S",
    WindAngle >= 225 & WindAngle < 315 ~ "W"
  ),
  Temperature = rnorm(n_rows, 
                      mean = 15 + 10 * sin(2 * pi * (as.numeric(dates) - as.numeric(min(dates))) / (365 * 24 * 3600)), 
                      sd = 3),
  RelativeHumidity = pmin(100, pmax(0, rnorm(n_rows, mean = 70, sd = 15))),
  Shabnam_temp = rnorm(n_rows, mean = -10, sd = 1), # Derived temperature measurement
  Pressure = rnorm(n_rows, mean = 1013, sd = 5),
  Vision = pmax(0, rnorm(n_rows, mean = 10, sd = 2)),
  Cloud = sample(c("Clear", "Partly Cloudy", "Cloudy", "Overcast"), 
                 n_rows, replace = TRUE, 
                 prob = c(0.3, 0.3, 0.2, 0.2))
)

theme = bs_theme(bootswatch = "darkly", 
                 bg = "#222222", 
                 fg = "#86C7ED", 
                 success ="#86C7ED")

ui <- page_sidebar(
  title = "Airport Weather Station Dashboard",
  sidebar = sidebar(
    date_range = dateRangeInput("dateRange", "Select Date Range",
                                start = min(weather_data$Date),
                                end = max(weather_data$Date)),
    selectInput("plotType", "Select Plot Type",
                choices = c("Temperature" = "temp",
                            "Wind Speed" = "wind",
                            "Pressure" = "pressure",
                            "Humidity" = "humidity")),
    checkboxInput("showCloud", "Show Cloud Coverage", FALSE)
  ),
  
  layout_columns(
    values = 1,
    card(
      card_header("Weather Trends"),
      plotOutput("weatherPlot")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Current Conditions"),
        value_box(
          title = "Temperature",
          value = textOutput("currentTemp"),
          showcase = bsicons::bs_icon("thermometer-half")
        )
      ),
      card(
        card_header("Wind Information"),
        value_box(
          title = "Wind Speed",
          value = textOutput("currentWind"),
          showcase = bsicons::bs_icon("wind")
        )
      )
    ),
    card(
      card_header("Weather Statistics"),
      tableOutput("weatherStats")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    weather_data %>%
      filter(Date >= input$dateRange[1],
             Date <= input$dateRange[2])
  })
  
  output$weatherPlot <- renderPlot({
    data <- filtered_data()
    
    p <- switch(input$plotType,
                "temp" = ggplot(data, aes(x = Date, y = Temperature)) +
                  geom_line(color = "red") +
                  labs(y = "Temperature (°C)"),
                "wind" = ggplot(data, aes(x = Date, y = WindSpeed)) +
                  geom_line(color = "blue") +
                  labs(y = "Wind Speed (m/s)"),
                "pressure" = ggplot(data, aes(x = Date, y = Pressure)) +
                  geom_line(color = "purple") +
                  labs(y = "Pressure (hPa)"),
                "humidity" = ggplot(data, aes(x = Date, y = RelativeHumidity)) +
                  geom_line(color = "green") +
                  labs(y = "Relative Humidity (%)")
    )
    
    if (input$showCloud) {
      p <- p + geom_point(aes(color = Cloud), alpha = 0.5)
    }
    
    p + theme_minimal() +
      labs(title = "Weather Trends",
           x = "Date")
  })
  
  output$currentTemp <- renderText({
    last_record <- tail(filtered_data(), 1)
    paste0("%.1f°C", last_record$Temperature)
  })
  
  output$currentWind <- renderText({
    last_record <- tail(filtered_data(), 1)
    sprintf("%.1f m/s\n%s", last_record$WindSpeed, last_record$WindDirection)
  })
  
  output$weatherStats <- renderTable({
    filtered_data() 
  })
}

shinyApp(ui, server)
