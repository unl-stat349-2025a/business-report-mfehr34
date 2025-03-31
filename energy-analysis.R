# Import libraries
library(tidyverse)
library(lubridate)

# Load in Energy dataset
energy <- read.csv("SOCO Energy Data 2425.csv")
head(energy)


### Data Transformation ###

# Only keep relevant fields
energy <- energy %>% 
  select(-BA_Code, -Demand_Forecast, -Total_Interchange, -Net_Generation)
head(energy)

summary(energy)

energy[is.na(energy$Timestamp), ]


# Remove 'Central Time' from Timestamp
energy <- energy %>% 
  mutate(Timestamp = str_replace(Timestamp, ", Central Time", ""))
head(energy)

energy

# Change Timestamp to correct data type
energy$Timestamp <- as.POSIXct(energy$Timestamp, format="%m/%d/%Y")

summary(energy)

# Create Year column and Month column
energy <- energy %>%
  mutate(Year = year(Timestamp))
energy <- energy %>%
  mutate(Month = month(Timestamp))
energy$Year <- as.numeric(energy$Year)
energy$Month <- as.factor(energy$Month)

head(energy)


############################################


### EDA

# Cleaned dataset (no missing values)
summary(energy)

# Standard deviation
sd(energy$Demand)
# Maximum
energy[energy$Demand == max(energy$Demand), ]
# Minimum
energy[energy$Demand == min(energy$Demand), ]


# Plotting Demand over time
ggplot(energy, aes(x=Timestamp, y=Demand)) +
  geom_line() +
  labs(title = "Electricity Demand Over Time",
       x = "Timestamp", y = "Demand (MWh)")



############################################


### Linear Model


lm_model <- lm(Demand ~ Year + Month, data = energy)

# Summary of the model (to see main effects)
summary(lm_model)

# Create data frame for the next 5 years (2026-2030)
future_years <- c(2025, 2026, 2027, 2028, 2029)
future_months <- factor(1:12) # Ensure months are 01, 02, ..., 12

future_data <- expand.grid(Year = future_years, Month = future_months)

# Make predictions for the next 5 years
predictions <- predict(lm_model, newdata = future_data, se.fit = TRUE)

# Store prediction data in future_data
future_data$Pred_Demand<- predictions$fit
future_data$Pred_SE <- predictions$se.fit

# Visualize future predictions
future_data <- future_data %>% arrange(Year)
future_data <- future_data %>%
  mutate(Date = paste(Year, Month, '01', sep='-'))
future_data$Date <- as.POSIXct(future_data$Date, format = "%Y-%m-%d", tz = "UTC")
head(future_data) 

ggplot(future_data, aes(x=Date, y=Pred_Demand)) +
  geom_line() +
  labs(title = "Projected Electricity Demand Over the Next 5 Years",
       x = "Time", y = "Demand (Mwh)")



