# Import libraries
library(tidyverse)

# Import the Dauphin Islsand Water Temp datasets
col_widths <- c(4,3,3,3,3,5,5,4,6,6,6,5,6,6,6,6,5,6)
col_names <- c('YY','MM','DD','hh','mm','WDIR','WSPD','GST','WVHT','DPD','APD',
               'MWD','PRES','ATMP','WTMP','DEWP','VIS','TIDE')
dauphin2022 <- read.fwf("data/2022_DauphinIsland2_Data.txt", widths=col_widths,
                        skip=1, col.names=col_names, colClasses='character')
head(dauphin2022)
dauphin2023 <- read.fwf("data/2023_DauphinIsland2_Data.txt", widths=col_widths,
                        skip=1, col.names=col_names, colClasses='character')
head(dauphin2023)
dauphin2024 <- read.fwf("data/2024_DauphinIsland2_Data.txt", widths=col_widths,
                        skip=1, col.names=col_names, colClasses='character')
head(dauphin2024)

### Data Transformation ###

# Keep only the relevant fields and combine the datasets into one for each buoy
dauphin2022 <- dauphin2022 %>% select(YY, MM, DD, hh, mm, WTMP)
dauphin2023 <- dauphin2023 %>% select(YY, MM, DD, hh, mm, WTMP)
dauphin2024 <- dauphin2024 %>% select(YY, MM, DD, hh, mm, WTMP)

watertemp <- bind_rows(dauphin2022, dauphin2023, dauphin2024)
summary(watertemp)

## Create Timestamp column by combining YY, MM, DD, hh, and mm columns

# First, remove extra whitespaces
watertemp$YY <- str_trim(watertemp$YY, side='both')
watertemp$MM <- str_trim(watertemp$MM, side='both')
watertemp$DD <- str_trim(watertemp$DD, side='both')
watertemp$hh <- str_trim(watertemp$hh, side='both')
watertemp$mm <- str_trim(watertemp$mm, side='both')

# Only keep one record per hour
watertemp <- watertemp[(watertemp$mm %in% c('00')), ]

# Then, create date column and time column
watertemp <- watertemp %>%
  mutate(Date = paste(YY, MM, DD, sep='-'))
watertemp <- watertemp %>%
  mutate(Time = paste(hh, mm, sep=':'))
head(watertemp)

# Finally, combine the columns into one and get rid of redundant columns
watertemp <- watertemp %>%
  mutate(Timestamp = paste(Date, Time, sep=' '))
watertemp$Timestamp <- as.POSIXct(watertemp$Timestamp, format="%Y-%m-%d %H:%M", tz='UTC')
watertemp$Year <- as.numeric(watertemp$YY)
watertemp$Month <- as.factor(watertemp$MM)

# Change water temperature to degrees Fahrenheit
watertemp$WTMP <- as.double(watertemp$WTMP)
watertemp <- watertemp %>%
  mutate(WTMP_F = WTMP * 1.8 + 32)

# Only keep necessary columns
watertemp <- watertemp %>% select(Timestamp, Year, Month, WTMP_F)
head(watertemp)

# Plot with missing values
plot(watertemp$Timestamp, watertemp$WTMP_F)

# Rows with missing timestamps
watertemp[is.na(watertemp$Timestamp), ]

# Proportion of missing water temp values (1830.2 is 999.0 celsius but in fahrenheit)
sum(watertemp$WTMP_F == 1830.2) / nrow(watertemp)
# Not very many, so drop missing values
watertemp <- watertemp[(watertemp$WTMP != 1830.2), ]


############################################


### EDA

# Cleaned Dataset
summary(watertemp)

# Standard deviation
sd(watertemp$WTMP_F)
# Maximum
watertemp[watertemp$WTMP_F == max(watertemp$WTMP_F), ]
# Minimum
watertemp[watertemp$WTMP_F == min(watertemp$WTMP_F), ]

# Plotting temperature over time with horizontal line at operating threshold
ggplot(watertemp, aes(x = Timestamp, y = WTMP_F)) +
  geom_line() +
  geom_hline(yintercept=85, color="red", linetype="dashed", linewidth=0.5) +
  labs(title = "Water Temp Over Time", x = "Date", y = "Temperature (°F)")


############################################


### Linear Model


lm_model <- lm(WTMP_F ~ Year + Month, data = watertemp)

# Summary of the model (to see main effects)
summary(lm_model)

# Create data frame for the next 5 years (2026-2030)
future_years <- c(2025, 2026, 2027, 2028, 2029)
future_months <- factor(sprintf("%02d", 1:12)) # Ensure months are 01, 02, ..., 12

future_data <- expand.grid(Year = future_years, Month = future_months)

# Make predictions for the next 5 years
predictions <- predict(lm_model, newdata = future_data, se.fit = TRUE)

# Store prediction data in future_data
future_data$Pred_WTMP <- predictions$fit
future_data$Pred_SE <- predictions$se.fit
# Calculate prob. that the water temperature is above 85°F for each month/year
future_data <- future_data %>%
  mutate(prob_above = 1 - pnorm(85, mean = Pred_WTMP, sd = Pred_SE))

# See which predictions exceed 85°F and their prob.
above_85 <- future_data %>% filter(Pred_WTMP > 85)
print(above_85)

# Visualize future predictions
future_data <- future_data %>% arrange(Year)
future_data <- future_data %>%
  mutate(Date = paste(Year, Month, '01', sep='-'))
future_data$Date <- as.POSIXct(future_data$Date, format = "%Y-%m-%d", tz = "UTC")
head(future_data) 

ggplot(future_data, aes(x=Date, y=Pred_WTMP)) +
  geom_line() +
  geom_hline(yintercept = 85, linetype = "dashed", color = "red") +
  labs(title = "Projected Water Temperature Over the Next 5 Years",
       x = "Time (Year)", y = "Water Temperature (°F)")
