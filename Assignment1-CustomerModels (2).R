library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(caret)
library(car)
library(nnet)
library(effects)
library(mlogit)
library(lmtest)
library(AER)
library(mfx)
library(ROCR)

setwd("/Users/erlanggaroesdyoko/Documents/MADS/4th Block/Customer Models/Assignment 1/R Studio")
df <- read.csv("cm_car_sales_data_2425.csv")

head(df)
summary(df)
max(df$customer_id)

unique(df$state)

# missing value
sum(is.na(df))
colSums(is.na(df))

# repeated customers
repeat_customers <- df %>%
  count(customer_id) %>%
  filter(n > 1) %>%
  pull(customer_id)

repeat_customers_df <- df %>%
  filter(customer_id %in% repeat_customers)

# View the result
#View(repeat_customers_df)

# duplicated customers
df[duplicated(df) | duplicated(df, fromLast = TRUE), ]

# Remove duplicate customers
df <- df[!duplicated(df), ]

# Descriptive Plots ----
# Ensure 'sold' and 'trade_in_fuel_type' are factors
df$sold <- as.factor(df$sold)
df$brand <- as.factor(df$brand)
df$trade_in_fuel_type <- as.factor(df$trade_in_fuel_type)
df$state <- as.factor(df$state)
df$dealership_location <- as.factor(df$dealership_location)
df$dealership_size <- as.factor(df$dealership_size)
df$customer_sex <- as.factor(df$customer_sex)

# Data Transformation for Customer Sex and State
df$sex_male <- ifelse(df$customer_sex == "Male", 1, 0)
df$state_NY <- ifelse(df$state == "New York", 1, 0)
df$state_CA <- ifelse(df$state == "California", 1, 0)
df$state_OR <- ifelse(df$state == "Oregon", 1, 0)
df$state_TX <- ifelse(df$state == "Texas", 1, 0)

# Brand
ggplot(df, aes(x = brand, fill = sold)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Sales based on Brands", x = "Brand", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Trade In Type
ggplot(df, aes(x = trade_in_fuel_type, fill = sold)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Sales based on Trade In Vehicle Types", x = "Vehicle Type", y = "Count") +
  theme_minimal()

# Transmission Type
ggplot(df, aes(x = trade_in_transmission_type, fill = sold)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Sales based on Vehicles Transmission Types", x = "Transmission Type", y = "Count") +
  theme_minimal()

# State
ggplot(df, aes(x = state, fill = sold)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Sales based on States", x = "State", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Dealership Location Type
ggplot(df, aes(x = dealership_location, fill = sold)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Sales based on Location Type", x = "State", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Dealership size
ggplot(df, aes(x = dealership_size, fill = sold)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Sales based on Dealership Size", x = "State", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gender
ggplot(df, aes(x = customer_sex, fill = sold)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Sales based on Gender", x = "Gender", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Dealership Experience
ggplot(df, aes(x = dealership_years_experience, fill = sold)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
  labs(title = "Distribution of Dealership Experience by Sale Outcome",
       x = "Dealership Experience",
       y = "Count") +
  theme_minimal()

ggplot(df, aes(x = dealership_years_experience, color = sold, fill = sold)) +
  geom_density(alpha = 0.4) +
  labs(title = "Density of Dealership Experience by Sale Outcome",
       x = "Dealership Experience",
       y = "Density") +
  theme_minimal()

ggplot(df, aes(x = dealership_years_experience, fill = sold)) +
  geom_histogram(aes(y = after_stat(density)), position = "identity", bins = 30, alpha = 0.5) +
  labs(title = "Proportional Histogram of Dealership Experience by Sale Outcome",
       x = "Dealership Experience", y = "Density") +
  theme_minimal()

# Seller Experience
ggplot(df, aes(x = seller_experience, fill = sold)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
  labs(title = "Distribution of Seller Experience by Sale Outcome",
       x = "Seller Experience",
       y = "Count") +
  theme_minimal()

ggplot(df, aes(x = seller_experience, color = sold, fill = sold)) +
  geom_density(alpha = 0.4) +
  labs(title = "Density of Seller Experience by Sale Outcome",
       x = "Seller Experience",
       y = "Density") +
  theme_minimal()

ggplot(df, aes(x = seller_experience, fill = sold)) +
  geom_histogram(aes(y = after_stat(density)), position = "identity", bins = 30, alpha = 0.5) +
  labs(title = "Proportional Histogram of Seller Experience by Sale Outcome",
       x = "Seller Experience", y = "Density") +
  theme_minimal()

# Trade in Mileage
ggplot(df, aes(x = trade_in_mileage, fill = sold)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
  labs(title = "Distribution of Trade In Mileage by Sale Outcome",
       x = "Trade In Mileage",
       y = "Count") +
  theme_minimal()

ggplot(df, aes(x = trade_in_mileage, color = sold, fill = sold)) +
  geom_density(alpha = 0.4) +
  labs(title = "Density of Trade In Mileage by Sale Outcome",
       x = "Trade In Mileage",
       y = "Density") +
  theme_minimal()

ggplot(df, aes(x = trade_in_mileage, fill = sold)) +
  geom_histogram(aes(y = after_stat(density)), position = "identity", bins = 30, alpha = 0.5) +
  labs(title = "Proportional Histogram of Trade In Mileage by Sale Outcome",
       x = "Trade In Mileage", y = "Density") +
  theme_minimal()

# Customer Income
ggplot(df, aes(x = customer_income, fill = sold)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
  labs(title = "Distribution of Customer Income by Sale Outcome",
       x = "Customer Income",
       y = "Count") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()

ggplot(df, aes(x = log10(customer_income), fill = sold)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
  labs(title = "Log-Scaled Customer Income Distribution by Sale Outcome",
       x = "Log10(Consumer Income)",
       y = "Count") +
  theme_minimal()

# Price
ggplot(df, aes(x = price, fill = sold)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
  labs(title = "Distribution of Price by Sale Outcome",
       x = "Price",
       y = "Count") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()

ggplot(df, aes(x = log10(customer_income), fill = sold)) +
  geom_histogram(position = "dodge", bins = 30, alpha = 0.7) +
  labs(title = "Log-Scaled Price Distribution by Sale Outcome",
       x = "Log10(Price)",
       y = "Count") +
  theme_minimal()

ggplot(df, aes(x = price, color = sold, fill = sold)) +
  geom_density(alpha = 0.4) +
  labs(title = "Density of Price by Sale Outcome",
       x = "Price",
       y = "Density") +
  theme_minimal()

# Brand
ggplot(df, aes(x = apple_carplay_or_android_auto, fill = sold)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Sales based on Carplay Features", x = "Carplay Features", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Look for Outliers ----
# Customer Income - POTENTIAL OUTLIERS
ggplot(df, aes(x = sold, y = customer_income, fill = sold)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.6) +
  labs(title = "Consumer Income by Sale Outcome",
       x = "Sold (0 = Not Sold, 1 = Sold)",
       y = "Consumer Income") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Dealership Experience
ggplot(df, aes(x = sold, y = dealership_years_experience, fill = sold)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.6) +
  labs(title = "Dealership Experience by Sale Outcome",
       x = "Sold (0 = Not Sold, 1 = Sold)",
       y = "Dealership Experience (Years)") +
  theme_minimal()

# Seller Experience
ggplot(df, aes(x = sold, y = seller_experience, fill = sold)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.6) +
  labs(title = "Seller Experience by Sale Outcome",
       x = "Sold (0 = Not Sold, 1 = Sold)",
       y = "Seller Experience (Years)") +
  theme_minimal()

# Trade In Value
ggplot(df, aes(x = sold, y = trade_in_value, fill = sold)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.6) +
  labs(title = "Trade In Value by Sale Outcome",
       x = "Sold (0 = Not Sold, 1 = Sold)",
       y = "Trade in Value") +
  theme_minimal()

# Trade In Mileage - POTENTIAL OUTLIERS
ggplot(df, aes(x = sold, y = trade_in_mileage, fill = sold)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.6) +
  labs(title = "Trade In Mileage by Sale Outcome",
       x = "Sold (0 = Not Sold, 1 = Sold)",
       y = "Trade in Mileage") +
  theme_minimal()

# Price - POTENTIAL OUTLIERS
ggplot(df, aes(x = sold, y = price, fill = sold)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.6) +
  labs(title = "Price by Sale Outcome",
       x = "Sold (0 = Not Sold, 1 = Sold)",
       y = "Price") +
  theme_minimal()

# Customer Age
ggplot(df, aes(x = sold, y = customer_age, fill = sold)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.6) +
  labs(title = "Customer Age by Sale Outcome",
       x = "Sold (0 = Not Sold, 1 = Sold)",
       y = "Customer Age") +
  theme_minimal()


# Training / Test Split before model evaluation ----

set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(df), 0.80 * nrow(df))  # 80% training
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Q2 ----

## Logistic Regression model based on hypotheses / theory (basemodel)
Logistic_regression <- glm(sold ~ brand + price + battery_health_report + fast_charging_capabilities + regenerative_braking_system +
                           subzero_weather_package + customer_age + customer_sex + state, family="binomial"(link="logit"), data=train_data)
summary(Logistic_regression)
exp(coef(Logistic_regression))

# marginal effects
logitmfx(Logistic_regression,data=train_data)

## Complex Model - including All variables ----
Complex_model <- glm(sold ~ . - range_estimator - glass_roof - trade_in_model - car_models -sex_male - state,family="binomial"(link="logit"), data=train_data)
summary(Complex_model)

logitmfx(Complex_model,data=train_data)


## Predict on Test data ----
# Base Model
test_data$pred_base <- predict(Logistic_regression, newdata = test_data, type = "response")

# Complex Model
test_data$pred_complex <- predict(Complex_model, newdata = test_data, type = "response")

## Hit rate of 2 models ----

# Binary predictions (threshold = 0.5)
test_data$pred_base_class <- ifelse(test_data$pred_base > 0.5, 1, 0)
test_data$pred_complex_class <- ifelse(test_data$pred_complex > 0.5, 1, 0)

# Confusion matrices
conf_base <- table(Actual = test_data$sold, Predicted = test_data$pred_base_class)
conf_complex <- table(Actual = test_data$sold, Predicted = test_data$pred_complex_class)

# Hit rates
hit_rate_base <- sum(diag(conf_base)) / nrow(test_data)
hit_rate_complex <- sum(diag(conf_complex)) / nrow(test_data)

cat("Hit Rate (Base Model):", hit_rate_base, "\n",
    "Hit Rate (Complex Model):", hit_rate_complex, "\n")

## Lift Curves ----
makeLiftPlot <- function(Prediction, Evaluate, ModelName){
  iPredictionsSorted <- sort(Prediction,index.return=T,decreasing=T)[2]$ix #extract the index order according to predicted 1's
  CustomersSorted <- Evaluate[iPredictionsSorted] #sort the true behavior of customers according to predictions
  SumChurnReal<- sum(Evaluate == 1) #total number of real 1's in the evaluation set
  CustomerCumulative=seq(length(Evaluate))/length(Evaluate) #cumulative fraction of customers
  ChurnCumulative=apply(matrix(CustomersSorted==1),2,cumsum)/SumChurnReal #cumulative fraction of 1's
  ProbTD = sum(CustomersSorted[1:floor(length(Evaluate)*.1)]==1)/floor(length(Evaluate)*.1) #probability of 1 in 1st decile
  ProbOverall = SumChurnReal / length(Evaluate) #overall probability of 1's
  TDL = ProbTD / ProbOverall
  GINI = sum((ChurnCumulative-CustomerCumulative)/(t(matrix(1,1,length(Evaluate))-CustomerCumulative)),na.rm=T)/length(Evaluate)
  plot(CustomerCumulative,ChurnCumulative,type="l",main=paste("Lift curve of", ModelName),xlab="Cumulative fraction of customers (sorted by predicted probability of 1's)",ylab="Cumulative fraction of real 1's")
  grid()
  lines(c(0,1),c(0,1),col="blue",type="l",pch=22, lty=2)
  legend(.66,.2,c("According to model","Random selection"),cex=0.8,  col=c("black","blue"), lty=1:2)
  text(0.15,1,paste("TDL = ",round(TDL,2), "; GINI = ", round(GINI,2) ))
  return(data.frame(TDL,GINI))
}

# Run lift plot for base model
results_base <- makeLiftPlot(test_data$pred_base, test_data$sold, "Base Model")

# Run lift plot for complex model
results_complex <- makeLiftPlot(test_data$pred_complex, test_data$sold, "Complex Model")

## AIC of both models ----

AIC(Logistic_regression)
AIC(Complex_model)

## LR test ----
# Likelihood Ratio Test
lr_result <- lrtest(Logistic_regression, Complex_model)
print(lr_result)


# Model Q3 ----
# Filter training data: only sold cars
df_sold <- subset(train_data, sold == 1)

# Ensure brand levels are consistent
df_sold$brand <- factor(df_sold$brand, levels = c("Toyota", "Ford", "Tesla"))

# Convert to mlogit data format
data_mlogit <- mlogit.data(df_sold, choice = "brand", shape = "wide")

# Fit the multinomial logit model
model_MNL <- mlogit(
  brand ~ 0 | price + battery_health_report + fast_charging_capabilities + 
    regenerative_braking_system + subzero_weather_package + customer_age + 
    sex_male + state_NY + state_CA + state_OR + state_TX,
  data = data_mlogit,
  reflevel = "Toyota"
)

summary(model_MNL)


# Prediction
# Filter and prep test data (sold == 1)
test_sold <- subset(test_data, sold == 1)
test_sold$brand <- factor(test_sold$brand, levels = c("Toyota", "Ford", "Tesla"))

# Convert to mlogit format
test_mlogit <- mlogit.data(test_sold, choice = "brand", shape = "wide")

# Predict probabilities
pred_probs <- predict(model_MNL, newdata = test_mlogit)

# Get predicted brand (choice with highest probability for each individual)
test_sold$predicted_brand <- apply(pred_probs, 1, function(row) {
  colnames(pred_probs)[which.max(row)]
})

# Convert to factor to match brand variable
test_sold$predicted_brand <- factor(test_sold$predicted_brand, levels = c("Toyota", "Ford", "Tesla"))

# Hit Rate
# Ensure ground truth brand factor is consistent
test_sold$brand <- factor(test_sold$brand, levels = c("Toyota", "Ford", "Tesla"))

# Confusion matrix
hr_table <- table(Predicted = test_sold$predicted_brand, Actual = test_sold$brand)

# Hit rate
hit_rate <- sum(diag(hr_table)) / sum(hr_table)

# Output
print(hr_table)
cat("Hit rate:", round(hit_rate, 4), "\n")

# LR Test with Simple model
# Fit the multinomial logit model
model_MNL_2 <- mlogit(
  brand ~ 0 | price,
  data = data_mlogit,
  reflevel = "Toyota"
)
lrtest(model_MNL_2, model_MNL)

# Q4 ----
# plot to see censoring on trade in value
hist(df$trade_in_value,
     main = "Distribution of Trade In Value",
     xlab = "Trade In Value (in USD)",
     col = "lightblue", 
     border = "black") # this shows the trade in value is right-censored

# tobit type 1 model to handle censoring
tobit_trade_in <- tobit(trade_in_value~ + trade_in_mileage + trade_in_exterior_condition +
                        trade_in_fuel_type + trade_in_tire_condition + trade_in_transmission_type +
                        dealership_location + dealership_size + dealership_years_experience 
                        , data = train_data, right = 30000) 
summary(tobit_trade_in)

# OLS model as comparison
ols_trade_in <- lm(
  trade_in_value ~ trade_in_mileage + trade_in_exterior_condition +
    trade_in_fuel_type + trade_in_tire_condition + trade_in_transmission_type +
    dealership_location + dealership_size + dealership_years_experience,
  data = train_data)
summary(ols_trade_in)

# Tobit and OLS model results show the same significant variables, however the coefficient estimates is higher (right censoring) and more approraite for tobit

