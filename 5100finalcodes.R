# Load necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(hrbrthemes)
library(viridis)
library(caret)
library(corrplot)
library(randomForest)

# Load data
ad_e <- read_csv("Ad_table_(extra).csv")
data <- ad_e

# Remove 'mpg' from Average_mpg column
data$Average_mpg <- gsub(" mpg", "", data$Average_mpg)

# Remove 'mph' from Top_speed column
data$Top_speed <- gsub(" mph", "", data$Top_speed)

# Convert columns to numeric
data$Runned_Miles <- as.numeric(data$Runned_Miles)
data$Annual_Tax <- as.numeric(data$Annual_Tax)
data$Average_mpg <- as.numeric(data$Average_mpg)
data$Top_speed <- as.numeric(data$Top_speed)

# Check for missing data
missing_data <- data %>%
  summarise_all(~sum(is.na(.)))

# Check for missing data percentages
missing_data_percentages <- data %>%
  summarise_all(~sum(is.na(.))/nrow(data))




# Unique vehicle brands
brand <- unique(data$Maker)

# Loop through each unique brand and count models
for (i in brand) {
  cat("Brand:", i, "\n")
  model <- unique(data$Genmodel[data$Maker == i])
  number_of_models <- length(model)
  cat("Number of Models:", number_of_models, "\n\n")
}

# Unique colors, gearbox types, body types, and fuel types
unique_colors <- unique(na.omit(data$Color))
unique_gearbox <- unique(na.omit(data$Gearbox))
unique_bodytype <- unique(na.omit(data$Bodytype))
unique_fueltype <- unique(na.omit(data$Fuel_type))

# Summary statistics of the data
summary(data)

# Summary of data structure
glimpse(data)




# Data Visualization
# Number of Vehicles by Brand
ggplot(data, aes(x = Maker, fill = Maker)) +
  geom_bar(color = "black") +
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + 
  labs(title = "Number of Vehicles by Brand", x = "Brand Name", y = "Count")

# Distribution of vehicle prices with gearbox
ggplot(na.omit(data[c("Price", "Gearbox")]), aes(x = Price, fill = Gearbox)) +
  geom_histogram(binwidth = 2000) +
  theme_fivethirtyeight() +
  labs(title = "Distribution of vehicle prices with gearbox") +
  coord_cartesian(xlim = c(NA, 50000))

# Distribution of vehicle prices with body type
ggplot(na.omit(data[c("Price", "Bodytype")]), aes(x = Price, fill = Bodytype)) +
  geom_histogram(binwidth = 2000) +
  theme_fivethirtyeight() +
  labs(title = "Distribution of vehicle prices with body type") +
  coord_cartesian(xlim = c(NA, 50000))

# Distribution of vehicle prices with fuel type
ggplot(na.omit(data[c("Price", "Fuel_type")]), aes(x = Price, fill = Fuel_type)) +
  geom_histogram(binwidth = 2000) +
  theme_fivethirtyeight() +
  labs(title = "Distribution of vehicle prices with fuel type") +
  coord_cartesian(xlim = c(NA, 50000))

# Bubble Plot of Price vs Engine Power for Ford Vehicles
ggplot(na.omit(data_f[c("Price", "Engine_power", "Engin_size", "Genmodel")]), aes(x = Engine_power, y = Price, size = Engin_size, color = Genmodel)) +
  geom_point(alpha = 0.2) +
  scale_size(range = c(1, 10)) +
  theme_fivethirtyeight() +
  labs(title = "Bubble Plot of Price vs Engine Power for Ford Vehicles", x = "Engine Power", y = "Price") +
  guides(size = guide_legend(override.aes = list(alpha = 1)))


# Create dummy variables for Bodytype
dummy_data <- dummyVars(~ Bodytype, data = data)
transformed_data <- predict(dummy_data, newdata = data)
data <- cbind(data, transformed_data)

# Create dummy variables for Gearbox
dummy_data <- dummyVars(~ Gearbox, data = data)
transformed_data <- predict(dummy_data, newdata = data)
data <- cbind(data, transformed_data)

# Create dummy variables for Fuel_type
dummy_data <- dummyVars(~ Fuel_type, data = data)
transformed_data <- predict(dummy_data, newdata = data)
data <- cbind(data, transformed_data)

# Calculate 'Size' variable
data$Size <- data$Width * data$Length * data$Height
data$Size <- data$Size / 1000000000

# Select relevant columns for regression
data_m <- data[, -(1:7)]
data_m <- data_m[, -2]
data_m <- data_m[, -(4:5)]

# Calculate Z-scores for 'Price' variable
z_scores <- scale(data_m$Price)

# Define a threshold for outliers
z_threshold <- 3

# Identify and remove outliers based on Z-scores
outliers <- which(abs(z_scores) > z_threshold)
data_m <- data_m[-outliers, ]

# Perform linear regression
lm <- lm(log(Price) ~ . , data = data_m)
summary(lm)




# Calculate residuals
residuals <- resid(lm)

# Q-Q plot of residuals
qqnorm(residuals)
qqline(residuals, col = "red")

# Histogram of residuals
ggplot(data.frame(Residuals = residuals), aes(x = Residuals)) +
  geom_histogram(binwidth = 0.001, fill = "blue", color = "black") + 
  xlim(c(-10, 10)) + 
  ggtitle("Histogram of Residuals") +
  xlab("Residuals") +
  ylab("Frequency")









