# Import libraries
library(tidyverse)
sessionInfo()

# Load in Energy dataset
energy <- read.csv("data/SOCO Energy Data 2425.csv")
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
  mutate(Month = month(Timestamp))
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
  labs(title = "Energy Demand Over Time",
       x = "Timestamp", y = "Demand (MWh)")



############################################


### Linear Model


lm_model <- lm(Demand ~ Month, data = energy)

# Summary of the model (to see main effects)
summary(lm_model)

# Create data frame to predict each month's demand
#future_years <- c(2025, 2026, 2027, 2028, 2029)
monthly_data <- expand.grid(Month = factor(1:12))
monthly_data

# Make predictions for the next 5 years
predictions <- predict(lm_model, newdata = monthly_data, se.fit = TRUE)

# Store prediction data in future_data
monthly_data$Pred_Demand<- predictions$fit
monthly_data$Pred_SE <- predictions$se.fit

# Visualize future predictions
monthly_data <- monthly_data %>% arrange(Month)
head(monthly_data)

ggplot(monthly_data, aes(x=as.numeric(Month), y=Pred_Demand)) +
  geom_line() +
  annotate("rect",
           xmin = 6, xmax = 9,
           ymin = -Inf, ymax = +Inf,
           fill = "steelblue", alpha = 0.2 ) +
  scale_x_continuous(breaks=1:12, labels=month.abb) +
  labs(title = "Energy Demand By Month",
       x = "Month", y = "Demand (MWh)")



