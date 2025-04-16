# Load necessary libraries
library(ggplot2)   # For data visualization
library(dplyr)     # For data manipulation
library(readr)     # For reading data
library(corrplot)  # For plotting correlation matrices

# Load the dataset from a URL
data <- read_csv("https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv")

# Display structure of the dataset (data types, number of observations, etc.)
str(data)

# Show summary statistics (mean, median, min, max, etc.) for each variable
summary(data)

# Remove rows with missing values, if any
data <- na.omit(data)

# Compute correlation matrix of the dataset
cor_matrix <- cor(data)

# Visualize the correlation matrix using colored tiles
corrplot(cor_matrix, method = "color", tl.cex = 0.6)

# Build a simple linear regression model to predict median house value (medv) using lstat
model_simple <- lm(medv ~ lstat, data = data)

# Display summary statistics of the simple linear regression model
summary(model_simple)

# Plot scatter plot of lstat vs medv with a fitted regression line
ggplot(data, aes(x = lstat, y = medv)) +
  geom_point(color = "blue") +                        # Plot data points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add regression line
  labs(title = "House Price vs LSTAT",                # Add title and axis labels
       x = "% Lower Status of Population",
       y = "Median House Price") +
  theme_minimal()                                     # Use minimal theme for clean look

# Build a multiple linear regression model using all predictors to predict medv
model_multi <- lm(medv ~ ., data = data)

# Display summary statistics of the multiple regression model
summary(model_multi)

# Predict house prices using the multiple regression model
predicted_prices <- predict(model_multi, data)

# Create a data frame comparing actual vs predicted prices (rounded to 2 decimals)
comparison <- data.frame(Actual = data$medv, Predicted = round(predicted_prices, 2))

# Display first few rows of the comparison table
head(comparison)

# Plot residuals of the multiple regression model to assess the fit
plot(model_multi$residuals,
     main = "Residuals",              # Plot title
     ylab = "Residuals",              # Y-axis label
     xlab = "Index",                  # X-axis label
     pch = 20,                        # Point style
     col = "darkgreen")              # Point color
abline(h = 0, col = "red")           # Add horizontal line at zero for reference
