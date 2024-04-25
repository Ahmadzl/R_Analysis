# Load library
library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)  
library(leaps)  
library(Metrics)


# The file path
file_path <- "C:/Users/AHMAD/Desktop/kunskapskontroll/Final/BMW.xlsx"


# Read Excel file 
data <- read_excel(file_path)


# First few rows of data
head(data)
# Summary data
summary(data)
# Structure data
str(data)
# View data
View(data) 


# Remove missing values
data_clean <- na.omit(data)

# Count missing values after removal
missing_count <- sum(is.na(data_clean))

# Count of missing values after removal
print(paste("Number of missing values after removal:", missing_count))

# Check column names
column_names <- colnames(data)
print(column_names)
str(data)

# Convert your data (Bensin=1, Diesel=2, Hybri=3)
data$Drivmenel <- ifelse(data$Drivmenel == "Bensin", 1,
                         ifelse(data$Drivmenel == "Diesel", 2,
                                ifelse(data$Drivmenel == "Hybri", 3, NA)))

# Convert the column to numeric type
data$Drivmenel <- as.numeric(data$Drivmenel)

# Convert your data (Automat=1, Manuell=2)
data$Växellåda <- ifelse(data$Växellåda == "Automat", 1,
                         ifelse(data$Växellåda == "Manuell", 2, NA))

# Convert the column to numeric type
data$Växellåda <- as.numeric(data$Växellåda)


# Removel your data (Datum, Link, Beskrivning)
data <- data[, !(colnames(data) %in% c("Datum", "Link", "Beskrivning"))]


str(data)


# Full Model --------------------------------------------------------------
full_mdl<-lm(data$Pris~., data = data) # with all variables 
summary(full_mdl)


# Null Model --------------------------------------------------------------
null_md1<- lm(data$Pris~1, data = data)
null_md1

# Stepwise ----------------------------------------------------------------
step_wise<-step(null_md1,scope = list(lower = null_md1, upper = full_mdl) ,direction = "both" , trace = 3)


# Fit Model 1
model1 <- lm(data$Pris ~ Årsmodell + Miltal + Modell, data = data)

# Fit Model 2
model2 <- lm(data$Pris ~ Årsmodell + Miltal + Modell + Växellåda, data = data)

# Calculate AIC for Model 1
aic_model1 <- AIC(model1)

# Calculate AIC for Model 2
aic_model2 <- AIC(model2)


# Compare AIC values
print (AIC(model1))
print (AIC(model2))

if (aic_model1 < aic_model2) {
  cat("Model 1 is better (lower AIC)\n")
} else if (aic_model2 < aic_model1) {
  cat("Model 2 is better (lower AIC)\n")
} else {
  cat("Both models are equally good (same AIC)\n")
}



# The model was used ------------------------------------------------
# Fit  linear regression model --------------------------------------------
lm_2 <- lm(Pris ~ ., data = data[, c("Årsmodell", "Drivmenel", "Miltal", "Växellåda", "Modell", "Pris")])

# Print the summary of model
summary(lm_2)

# Extract the independent variables from data 
new_data <- data[, c("Årsmodell", "Drivmenel", "Miltal", "Växellåda", "Modell")]

# Predictions (linear regression model)
predictions_m1 <- predict(lm_2, newdata = new_data)

# Predictions
print(predictions_m1)

par(mfrow = c(2, 2))
plot(lm_2)


# Predicted values with the actual values 
predicted_actual <- data.frame(Predicted = predictions_m1, Actual = data$Pris)

# Scatter plot
scatter_plot <- ggplot(predicted_actual, aes(x = Predicted, y = Actual)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Predicted Price", y = "Actual Price", title = "Predicted vs Actual Price") +
  theme_minimal()

# Print scatter plot
print(scatter_plot)

# Summary statistics of the predicted and actual values
summary(predicted_actual)
print(summary(lm_2))


# Scatter plot with confidence intervals
scatter_plot <- ggplot(predicted_actual, aes(x = Predicted, y = Actual)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1.5) +  # Increase line size
  geom_smooth(method = "lm", se = TRUE, color = "green") +  # Add confidence intervals
  labs(x = "Predicted Price", y = "Actual Price", title = "Predicted vs Actual Price") +
  theme_minimal()

# Print scatter plot
print(scatter_plot)

# line plot with confidence intervals
line_plot <- ggplot(predicted_actual, aes(x = Predicted, y = Actual)) +
  geom_line(color = rgb(0, 0, 255, maxColorValue = 255)) +  # Blue line
  geom_smooth(method = "lm", se = TRUE, color = rgb(255, 0, 0, maxColorValue = 255)) +  # Red line (confidence interval)
  labs(x = "Predicted Price", y = "Actual Price", title = "Predicted vs Actual Price") +
  theme_minimal()

# Print line plot
print(line_plot)
