install.packages("ggplot2")
install.packages("e1071")
library(e1071)
library(ggplot2)
library(dplyr)

#assign the data to a variable in the form of a dataframe

cars_data<-read.csv("C:\\Users\\VATTI VAMSHI\\Downloads\\archive\\Car_sales.csv")
#see the first 5 rows of data
head(cars_data)
#see the data types of different columns/data
str(cars_data)

#Univariate analysis for categorical Data
# Bar chart for Manufacturer
# Bar chart for Manufacturer with color variation
ggplot(cars_data, aes(x = Manufacturer, fill = Manufacturer)) +
  geom_bar() +
  labs(title = "Bar Chart for Manufacturer")
#Too many variables , hence we take average sales y each manufacturer

# Group by Manufacturer and calculate average sales
avg_sales <- cars_data %>%
  group_by(Manufacturer) %>%
  summarise(Avg_Sales = mean(Sales_in_thousands, na.rm = TRUE))

# Bar chart for Average Sales by Manufacturer
ggplot(avg_sales, aes(x = Manufacturer, y = Avg_Sales, fill = Manufacturer)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Sales  by Manufacturer")


# Bar chart for Vehicle_type with color variation
ggplot(cars_data, aes(x = Vehicle_type, fill = Vehicle_type)) +
  geom_bar() +
  labs(title = "Bar Chart for Vehicle Type")

# Bar chart for Fuel_efficiency with color variation
ggplot(cars_data, aes(x = Fuel_efficiency, fill = as.factor(Fuel_efficiency))) +
  geom_bar() +
  labs(title = "Bar Chart for Fuel Efficiency")

#Analysing Numerical Variables
#Plotting histograms for different numerical variables
hist(cars_data$Width, col = "blue", main = "Distribution of Width", xlab = "Width", probability = TRUE)
lines(density(cars_data$Width, na.rm = TRUE), col = "red", lwd = 2)

hist(cars_data$Sales_in_thousands, col = "blue", probability = TRUE)
lines(density(cars_data$Sales_in_thousands, na.rm = TRUE), col = "red", lwd = 2)


hist(cars_data$Sales_in_thousands, col = "blue", probability = TRUE)
lines(density(cars_data$Sales_in_thousands, na.rm = TRUE), col = "red", lwd = 2)

hist(cars_data$Engine_size, col = "blue", probability = TRUE)
lines(density(cars_data$Engine_size, na.rm = TRUE), col = "red", lwd = 2)

hist(cars_data$Horsepower, col = "blue", probability = TRUE)
lines(density(cars_data$Horsepower, na.rm = TRUE), col = "red", lwd = 2)

hist(cars_data$Wheelbase, col = "blue", probability = TRUE)
lines(density(cars_data$Wheelbase, na.rm = TRUE), col = "red", lwd = 2)

hist(cars_data$Width, col = "blue", probability = TRUE)
lines(density(cars_data$Width, na.rm = TRUE), col = "red", lwd = 2)

hist(cars_data$Length, col = "blue", probability = TRUE)
lines(density(cars_data$Length, na.rm = TRUE), col = "red", lwd = 2)

hist(cars_data$Curb_weight, col = "blue", probability = TRUE)
lines(density(cars_data$Curb_weight, na.rm = TRUE), col = "red", lwd = 2)

hist(cars_data$Fuel_capacity, col = "blue", probability = TRUE)
lines(density(cars_data$Fuel_efficiency, na.rm = TRUE), col = "red", lwd = 2)

hist(cars_data$Power_perf_factor, col = "blue", probability = TRUE)
lines(density(cars_data$Power_perf_factor, na.rm = TRUE), col = "red", lwd = 2)

#We need to normalize numerical data in order to build a regression model

#First we see if there are any null values and remove them-

# Check for missing values in each column
missing_values <- colSums(is.na(cars_data))
missing_values

# Display columns with missing values
columns_with_missing <- names(missing_values[missing_values > 0])

columns_with_missing

# Remove rows with missing values
cars_data <- cars_data[complete.cases(cars_data), ]

#Now we normalize numerical column using box-cox method-

# Load the MASS package
library(MASS)

colnames(cars_data)

# Extract numerical variables (assuming 'numerical_columns' contains the names of numerical columns)
numerical_columns <- cars_data[, c("Sales_in_thousands" ,
                                    "X__year_resale_value","Price_in_thousands",  
                                    "Engine_size","Horsepower", "Wheelbase",    
                                    "Width","Length","Curb_weight",         
                              "Fuel_capacity", "Fuel_efficiency",       
                                    "Power_perf_factor")]

str(numerical_columns)
#########################
# Normalize numerical variables using box-cox transformation
normalized_columns <- caret::preProcess(numerical_columns, method = c("BoxCox"))

# Apply normalization to the numerical columns
numerical_columns_normalized <- predict(normalized_columns, newdata = numerical_columns)

# Homoscedasticity - Check scatterplots of residuals vs. fitted values
# Assuming you have already built a linear regression model (replace 'model' with your actual model)
model <- lm(Sales_in_thousands ~ ., data = cars_data)
residuals <- residuals(model)

# Plot residuals vs. fitted values
plot(model$fitted.values, residuals, xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")

##Convert categorical data into numerical
categorical_columns <- c("Manufacturer", "Model", "Vehicle_type")

# Create dummy variables
install.packages("caret")
library(caret)
dummy_data <- dummyVars(" ~ .", data = cars_data[categorical_columns])

# Transform the dataset with dummy variables
numeric_data <- predict(dummy_data, newdata = cars_data)

# Combine the numeric data with the original dataset
converted_data <- cbind(cars_data[, -which(names(cars_data) %in% categorical_columns)], numeric_data)

# Display the first few rows of the converted dataset
head(converted_data)

# Check correlation matrix
cor_matrix <- cor(numerical_columns_normalized)
print(cor_matrix)


# Install and load the corrplot package
install.packages("corrplot")
library(corrplot)
options(repr.plot.width = 100, repr.plot.height = 50)

# Create a large and clear correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, mar = c(1, 1, 1, 1), addCoef.col = "black")


#############
# Extract variables with high correlation with X__year_resale_value
variables_to_include <- c("Sales_in_thousands", "Price_in_thousands", "Engine_size", "Horsepower", "Wheelbase", "Width", "Curb_weight", "Fuel_capacity", "Power_perf_factor")

# Build a linear regression model to predict X__year_resale_value
model_resale_value <- lm(X__year_resale_value ~ ., data = cars_data[, c(variables_to_include, "X__year_resale_value")])

# Display the summary of the model
summary(model_resale_value)

####################### From the model we

# Create a scatter plot
plot(cars_data$X__year_resale_value ~ cars_data$Price_in_thousands, 
     col = "red", pch = 16,
     main = "Relationship between X__year_resale_value and Variables",
     xlab = "Price_in_thousands", ylab = "X__year_resale_value")

# Add points for Curb_weight and Fuel_capacity
points(cars_data$X__year_resale_value ~ cars_data$Curb_weight, col = "blue", pch = 16)
points(cars_data$X__year_resale_value ~ cars_data$Fuel_capacity, col = "green", pch = 16)

# Add a legend
legend("topright", legend = c("Price_in_thousands", "Curb_weight", "Fuel_capacity"),
       col = c("red", "blue", "green"), pch = 16)


##########Testing regression model
# Select the variables for the model
variables_to_include <- c("Price_in_thousands", "Curb_weight", "Fuel_capacity", "X__year_resale_value")

# Subset the dataset with selected variables
selected_data <- converted_data[, variables_to_include]

# Assuming 'selected_data' is your dataset
set.seed(123)  # For reproducibility

# Create a binary variable indicating whether an observation is in the training set or not
selected_data$is_train <- ifelse(runif(nrow(selected_data)) < 0.8, 1, 0)

# Split the data into training and testing sets
train_data <- subset(selected_data, is_train == 1)
test_data <- subset(selected_data, is_train == 0)

# Build a linear regression model on the training set
model_test<- lm(X__year_resale_value ~ Price_in_thousands + Curb_weight + Fuel_capacity, data = train_data)

# Display the summary of the model used for testing
summary(model_test)

# Make predictions on the testing set
predictions <- predict(model_test, newdata = test_data)

# Evaluate the model performance
rmse <- sqrt(mean((test_data$X__year_resale_value - predictions)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Plot actual vs. predicted values
# Assuming 'predictions' is the vector of predicted values
predictions <- predict(model_test, newdata = test_data)

summary(predictions)

# Create a scatter plot for actual vs. predicted values
plot(test_data$X__year_resale_value, predictions, 
     xlab = "Actual X__year_resale_value", ylab = "Predicted X__year_resale_value",
     main = "Actual vs. Predicted X__year_resale_value")

# Add a 45-degree reference line
abline(a = 0, b = 1, col = "red")

# Add legend
legend("topright", legend = "Reference Line", col = "red", lty = 1)
