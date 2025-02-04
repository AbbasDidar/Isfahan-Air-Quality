
freq_max <- function(x){
  ifelse( is.na( sort( table( x[!is.na(x)] ), decreasing = TRUE )[1] ), NA, 
      names( sort( table( x[!is.na(x)] ), decreasing = TRUE )[1] )  )
}


Airport_Isfahan |> 
  group_by(Date) |> 
  summarise(
    WindSpeed = mean(WindSpeed, na.rm = TRUE),
    WindAngel = mean(WindAngel, na.rm = TRUE),
    Temperature = mean(Temperature, na.rm = TRUE),
    RelativityHumidity = mean(RelativityHumidity, na.rm = TRUE),
    Shabnam_temp = mean(Shabnam_temp, na.rm = TRUE),
    Pressure = mean(Pressure, na.rm = TRUE),
    Vision = mean(Vision, na.rm = TRUE),
    WindDirection = freq_max(WindDirection),
    Cloud = freq_max(Cloud)
  ) -> Daily_Mean


Airport_Isfahan |> 
  mutate(Month = substr(Date, 1, 7)) |> 
  group_by(Month) |> 
  summarise(
    WindSpeed_mean = mean(WindSpeed, na.rm = TRUE),
    WindSpeed_min = min(WindSpeed, na.rm = TRUE),
    WindSpeed_max = max(WindSpeed, na.rm = TRUE),
    WindSpeed_sd  = sd(WindSpeed, na.rm = TRUE),
    
    
    WindAngel_mean = mean(WindAngel, na.rm = TRUE),
    WindAngel_min = min(WindAngel, na.rm = TRUE),
    WindAngel_max = max(WindAngel, na.rm = TRUE),
    WindAngel_sd  = sd(WindAngel, na.rm = TRUE),
    
    Temperature_mean = mean(Temperature, na.rm = TRUE),
    Temperature_min = min(Temperature, na.rm = TRUE),
    Temperature_max = max(Temperature, na.rm = TRUE),
    Temperature_sd  = sd(Temperature, na.rm = TRUE),
    
    RelativityHumidity_mean = mean(RelativityHumidity, na.rm = TRUE),
    RelativityHumidity_min = min(RelativityHumidity, na.rm = TRUE),
    RelativityHumidity_max = max(RelativityHumidity, na.rm = TRUE),
    RelativityHumidity_sd  = sd(RelativityHumidity, na.rm = TRUE),
    
    Shabnam_temp_mean = mean(Shabnam_temp, na.rm = TRUE),
    Shabnam_temp_min = min(Shabnam_temp, na.rm = TRUE),
    Shabnam_temp_max = max(Shabnam_temp, na.rm = TRUE),
    Shabnam_temp_sd  = sd(Shabnam_temp, na.rm = TRUE),
    
    Pressure_mean = mean(Pressure, na.rm = TRUE),
    Pressure_min = min(Pressure, na.rm = TRUE),
    Pressure_max = max(Pressure, na.rm = TRUE),
    Pressure_sd  = sd(Pressure, na.rm = TRUE),
    
    Vision_mean = mean(Vision, na.rm = TRUE),
    Vision_min = min(Vision, na.rm = TRUE),
    Vision_max = max(Vision, na.rm = TRUE),
    Vision_sd  = sd(Vision, na.rm = TRUE),
    
    WindDirection = freq_max(WindDirection),
    Cloud = freq_max(Cloud)
  ) -> 



p1 <- ggplot(Monthly_Mean, aes(x = Month, y = WindSpeed, group = 1)) + 
  geom_line(color = "blue", size = 1) + 
  labs(title = "سرعت باد", x = "ماه", y = "سرعت باد (m/s)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# نمودار دما
p2 <- ggplot(Monthly_Mean, aes(x = Month, y = Temperature, group = 1)) + 
  geom_line(color = "red", size = 1) + 
  labs(title = "دما", x = "ماه", y = "دما (°C)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# نمودار فشار
p3 <- ggplot(Monthly_Mean, aes(x = Month, y = Pressure, group = 1)) + 
  geom_line(color = "green", size = 1) + 
  labs(title = "فشار", x = "ماه", y = "فشار (hPa)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p4 <- ggplot(Monthly_Mean, aes(x = Month, y = RelativityHumidity, group = 1)) + 
  geom_line(color = "green", size = 1) + 
  labs(title = "فشار", x = "ماه", y = "فشار (hPa)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p5 <- ggplot(Monthly_Mean, aes(x = Month, y = Shabnam_temp, group = 1)) + 
  geom_line(color = "green", size = 1) + 
  labs(title = "فشار", x = "ماه", y = "فشار (hPa)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p6 <- ggplot(Monthly_Mean, aes(x = Month, y = Vision, group = 1)) + 
  geom_line(color = "green", size = 1) + 
  labs(title = "فشار", x = "ماه", y = "فشار (hPa)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# نمایش نمودارها در کنار هم
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)



mean_speed <- mean(Monthly_Mean$WindSpeed_mean)
sd_speed <- sd(Monthly_Mean$WindSpeed_mean)
UCL <- mean_speed + 3 * sd_speed
LCL <- mean_speed - 3 * sd_speed

# رسم نمودار کنترل
library(ggplot2)

ggplot(Monthly_Mean, aes(x = Month, y = WindSpeed_mean, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(size = 2, color = "blue") +
  geom_hline(yintercept = mean_speed, color = "green", linetype = "dashed", size = 1) +
  geom_hline(yintercept = UCL, color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = LCL, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Control Chart for Wind Speed Mean",
       x = "Month",
       y = "Wind Speed Mean (m/s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


