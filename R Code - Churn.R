# Libraries ----
library(MASS)
library(dplyr) 
library(ggplot2)
library(ROCR)
library(rpart)
library(partykit)
library(gbm)
library(ipred)
library(caret)
library(randomForest)
library(e1071)
library (grid)


# Importing dataset & preperationg of training & test data----
setwd("C:/Users/nilsd/OneDrive/Documents/Data Science Methods for MADS")
data <- read.csv("data assignment 1.csv")

# Data Preparation ----
sum(is.na(data))

# Create the boxplot
# Reshape data into long format
data2 <- data[,-c(1,2,3,7,8,10,13,14)]
data_long <- pivot_longer(
  data2,
  cols = everything(),  # Include all variables
  names_to = "Variable",
  values_to = "Value"
)

# Create boxplots with facet_wrap
ggplot(data_long, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free") +  # Facet by variable
  theme_minimal() +
  labs(
    title = "Boxplots for Each Numerical Variable",
    x = "Variable",
    y = "Value"
  )

# Removoce of customer id
data <- data[,-c(1)]

# Training and test data split
set.seed(1234)
data$estimation_sample <-rbinom(nrow(data), 1, 0.75)

# Data Transformation
data$Gender <- as.factor(data$Gender)
data$Start_channel <- as.factor(data$Start_channel)
data$Province <- as.factor(data$Province)
data <- data %>%
  mutate(Home_label = case_when(
    Home_label == "G" ~ 1,
    Home_label == "F" ~ 2,
    Home_label == "E" ~ 3,
    Home_label == "D" ~ 4,
    Home_label == "C" ~ 5,
    Home_label == "B" ~ 6,
    Home_label == "A" ~ 7))
data$Home_label <- ordered(data$Home_label)  

training_data <- data[data$estimation_sample==1,]
test_data <- data[data$estimation_sample==0,]
training_data <- training_data[,-c(14)]
test_data <- test_data[,-c(14)]

data$Home_label <- as.numeric(data$Home_label)

comparison_df <- data.frame(Actual = test_data$Churn)


# Data Visualization & Exploration ----
# Transformation for individual plot
str(data$Home_label)
data$IncomeGroup <- cut(data$Income, breaks = c(0, 2000, 5000, 10000, 20000, 500000, 100000), labels = c("0-2K", "2K-5K", "5K-10K", "10K-20K", "20K-50K", "100K+"), 
                        right = FALSE)
data$IncomeGroup <- as.factor(data$IncomeGroup)

data$GasGroup <- cut(data$Gas_usage, 
                     breaks = c(0, 800, 1200, 1600, 2000, 2400, 2800, 3200, 3600, 4000, 10000, 50000), 
                     labels = c("<800", "800-1200", "1200-1600", "1600-2000", "2000-2400", "2400-2800", "2800-3200", "3200-3600", "3600-4000", "4000-10000", "10000>"), 
                     right = FALSE)

data$ElecGroup <- cut(data$Electricity_usage, 
                      breaks = c(0, 800, 1200, 1600, 2000, 2400, 2800, 3200, 3600, 4000, 10000, 50000), 
                      labels = c("<800", "800-1200", "1200-1600", "1600-2000", "2000-2400", "2400-2800", "2800-3200", "3200-3600", "3600-4000", "4000-10000", "10000>"), 
                      right = FALSE)

data$AgeGroup <- cut(data$Age, breaks = c(0, 18, 25, 35, 45, 55, 65, 105), 
                     labels = c("0-18", "18-25", "26-35", "36-45", "46-55", "56-65", "65+"), 
                     right = FALSE)
data$AgeGroup <- as.factor(data$AgeGroup)

## Explorative 1: Individual Plot ----

AgeGroupChurnPlot <- ggplot(data, aes(x = AgeGroup, fill = Churn == 1)) +
  geom_bar(position = "dodge") +
  labs(title = "Customer Churn by Age Group", x = "Age Group", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ProvinceChurnPlot <-ggplot(data, aes(x = Province, fill = Churn == 1)) +
  geom_bar(position = "dodge") +
  labs(title = "Customer Churn by Province", x = "Province", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

RelationLengthPlot <-ggplot(data, aes(x = as.factor(Churn), y = Relation_length, fill = as.factor(Churn))) +
  geom_violin() +
  labs(title = "Customer Churn by Relation Length", x = "Churn", y = "Relation Length") +
  theme_minimal()

ContractLengthPlot <-ggplot(data, aes(x = as.factor(Churn), y = Contract_length, fill = as.factor(Churn))) +
  geom_violin() +
  labs(title = "Customer Churn by Contract Length", x = "Churn", y = "Contract Length") +
  theme_minimal() 

GasUsagePlot <-ggplot(data, aes(x = GasGroup, fill = Churn == 1)) +
  geom_bar(position = "dodge") +
  labs(title = "Customer Churn by GasGroup", x = "GasGroup", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ElectricityPlot <-ggplot(data, aes(x = ElecGroup, fill = Churn == 1)) +
  geom_bar(position = "dodge") +
  labs(title = "Customer Churn by ElectricityGroup", x = "ElectricityGroup", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

IncomePlot <-ggplot(data, aes(x = IncomeGroup, fill = Churn == 1)) +
  geom_bar(position = "dodge") +
  labs(title = "Customer Churn by Income Group", x = "Income Group", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

HomeLabelPlot <-ggplot(data, aes(x = Home_label, fill = Churn == 1)) +
  geom_bar(position = "dodge") +
  labs(title = "Customer Churn by Home Label", x = "Home Label", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(hjust = 1))


grid.arrange(AgeGroupChurnPlot, ProvinceChurnPlot, IncomePlot, RelationLengthPlot, ContractLengthPlot, GasUsagePlot, ElectricityPlot,HomeLabelPlot, ncol = 3)

## Explorative 2: Statistical Test ----
# Statistical test: Wilcoxon for numerical and ANOVA/Chi-Square for categorical
test_variables <- function(data, target) {
  # Ensure target exists in the dataset
  if (!(target %in% names(data))) {
    stop(paste("Target variable", target, "does not exist in the dataset."))
  }
  
  # Ensure target is a factor
  data[[target]] <- as.factor(data[[target]])
  data[["Email_list"]] <- as.factor(data[["Email_list"]])
  data[["Gender"]] <- as.factor(data[["Gender"]])
  data[["Start_channel"]] <- as.factor(data[["Start_channel"]])
  
  # Separate numerical and categorical variables
  numerical_vars <- data[, sapply(data, is.numeric), drop = FALSE]
  categorical_vars <- data[, sapply(data, is.factor), drop = FALSE]
  
  # Remove the target variable from the subsets
  numerical_vars <- numerical_vars[, !(names(numerical_vars) %in% target), drop = FALSE]
  categorical_vars <- categorical_vars[, !(names(categorical_vars) %in% target), drop = FALSE]
  
  # Initialize lists to store results
  results <- list(numerical = list(), categorical = list())
  
  # Convert target to numeric for ANOVA
  data$target_numeric <- as.numeric(data[[target]]) - 1  # Convert factor levels (1,2) to (0,1)
  
  # Loop through numerical variables
  for (var in names(numerical_vars)) {
    # Wilcoxon test (non-parametric)
    formula <- as.formula(paste(var, "~", target))
    wilcox_test <- wilcox.test(formula, data = data)
    
    # Store results
    results$numerical[[var]] <- list(
      variable = var,
      p_value = wilcox_test$p.value,
      test = "Wilcoxon Rank-Sum Test"
    )
  }
  
  # Loop through categorical variables
  for (var in names(categorical_vars)) {
    # Count the number of levels in the categorical variable
    n_levels <- nlevels(data[[var]])
    
    if (n_levels > 2) {
      # Use ANOVA for variables with more than 2 levels
      formula <- as.formula(paste("target_numeric", "~", var))
      aov_test <- aov(formula, data = data)
      p_value <- summary(aov_test)[[1]][["Pr(>F)"]][1]  # Extract the p-value
      test_used <- "ANOVA Test"
    } else {
      # Use Chi-Square Test for variables with 2 levels
      table_data <- table(data[[var]], data[[target]])
      if (all(table_data >= 5)) {
        chi_test <- chisq.test(table_data)
        p_value <- chi_test$p.value
        test_used <- "Chi-Square Test"
      } else {
        # Fisher's Exact Test for small counts
        fisher_test <- tryCatch(
          fisher.test(table_data, workspace = 2e7, hybrid = TRUE, mult = 1e7),
          error = function(e) return(list(p.value = NA))
        )
        p_value <- fisher_test$p.value
        test_used <- "Fisher's Exact Test (with increased workspace)"
      }
    }
    
    # Store results
    results$categorical[[var]] <- list(
      variable = var,
      p_value = p_value,
      test = test_used
    )
  }
  
  # Convert results to data frames for easy viewing
  num_results <- do.call(rbind, lapply(results$numerical, as.data.frame))
  cat_results <- do.call(rbind, lapply(results$categorical, as.data.frame))
  
  # Combine results into one data frame
  final_results <- rbind(
    data.frame(type = "Numerical", num_results),
    data.frame(type = "Categorical", cat_results)
  )
  
  # Remove temporary column
  data$target_numeric <- NULL
  
  return(final_results)
}

# Use the original dataset, remove categorical groups used for plot
data_test <- data[, -c(14:18)]

# Run the test
results <- test_variables(data_test, target = "Churn")

# View results
print(results)

## Explorative 3: GLM
Logistic_regression_test <- glm(Churn ~ Gender + Age + Income +  Relation_length + Contract_length
                                + Start_channel + Email_list + Home_age + Home_label
                                + Electricity_usage + Gas_usage + Province, family="binomial", data=data)
summary(Logistic_regression_test)
vif_values <- vif(Logistic_regression_test)
print(vif_values)


predictions_model1 <- predict(Logistic_regression_test, type = "response", newdata=data)

# Check Multicollinearity ----
# Correlation Plot
library(corrplot)
corr_matrix <- cor(data_test %>% select_if(is.numeric))
corrplot(corr_matrix, method = "color", type = "upper", tl.col = "black", tl.cex = 0.7)

#VIF
library(car)
vif_model <- glm(Churn ~ Gender + Age + Income + Relation_length + Contract_length +
                   Email_list + Home_age + Electricity_usage + Gas_usage, 
                 data = data_test)
vif_values <- vif(vif_model)
print(vif_values)


# Logistic Regression ----

# Baseline Model ----
Baseline_model <- glm(Churn ~ Age + Income + Email_list + Relation_length, data = training_data, family = binomial)

summary(Baseline_model)

## Baseline Model validation criteria ----
# Fit Criteria
predictions_model1 <- predict(Baseline_model, type = "response", newdata=test_data)
comparison_df$prediction1 <- predictions_model1

#Hit Rate Table
predicted_model1 <- ifelse(predictions_model1>.5,1,0)
hit_rate_model1 <- table(test_data$Churn, predicted_model1, dnn= c("Observed", "Predicted"))
hit_rate_model1
(hit_rate_model1[1,1]+hit_rate_model1[2,2])/sum(hit_rate_model1)

#Decile lift
decile_predicted_model1 <- ntile(-predictions_model1, 10)
decile_model1 <- table(test_data$Churn, decile_predicted_model1, dnn= c("Observed", "Decile"))
decile_model1

(decile_model1[2,1] / (decile_model1[1,1] + decile_model1[2,1])) / mean(test_data$Churn)

#lift curve
pred_model1 <- prediction(predictions_model1, test_data$Churn)
perf_model1 <- performance(pred_model1,"tpr","fpr")
plot(perf_model1,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model1 <- performance(pred_model1,"auc")

#Gini
as.numeric(auc_model1@y.values)*2-1

# Stepwise model selection ----

# Full & Null Model Estimation
Logistic_regression_full <- glm(Churn ~ ., data = training_data, family = binomial)
Logistic_regression_null <- glm(Churn ~ 0, data = training_data, family = binomial)

## AIC Forward model ----
Logistic_regression_forward <- stepAIC(Logistic_regression_null, direction="forward", scope=list(lower=Logistic_regression_null, upper=Logistic_regression_full), trace = TRUE)

### AIC forward model validation criteria ----
# Fit Criteria
predictions_model2 <- predict(Logistic_regression_forward, type = "response", newdata=test_data)
comparison_df$prediction2 <- predictions_model2

#Hit Rate Table
predicted_model2 <- ifelse(predictions_model2>.5,1,0)
hit_rate_model2 <- table(test_data$Churn, predicted_model2, dnn= c("Observed", "Predicted"))
hit_rate_model2
(hit_rate_model2[1,1]+hit_rate_model2[2,2])/sum(hit_rate_model2)

#Decile lift
decile_predicted_model2 <- ntile(-predictions_model2, 10)
decile_model2 <- table(test_data$Churn, decile_predicted_model2, dnn= c("Observed", "Decile"))
decile_model2

(decile_model2[2,1] / (decile_model2[1,1]+ decile_model2[2,1])) / mean(test_data$Churn)

#lift curve
pred_model2 <- prediction(predictions_model2, test_data$Churn)
perf_model2<- performance(pred_model2,"tpr","fpr")
plot(perf_model2,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model2 <- performance(pred_model2,"auc")

#Gini

as.numeric(auc_model2@y.values)*2-1


Logistic_regression_backward <- stepAIC(Logistic_regression_full, direction="backward", trace = TRUE)

### AIC backward model validation criteria ----
# Fit Criteria
predictions_model3 <- predict(Logistic_regression_backward, type = "response", newdata=test_data)
comparison_df$prediction3 <- predictions_model3

#Hit Rate Table
predicted_model3 <- ifelse(predictions_model3>.5,1,0)
hit_rate_model3 <- table(test_data$Churn, predicted_model3, dnn= c("Observed", "Predicted"))
hit_rate_model3
(hit_rate_model3[1,1]+hit_rate_model3[2,2])/sum(hit_rate_model3)

#Decile lift
decile_predicted_model3 <- ntile(-predictions_model3, 10)
decile_model3 <- table(test_data$Churn, decile_predicted_model3, dnn= c("Observed", "Decile"))
decile_model3

(decile_model3[2,1] / (decile_model3[1,1]+ decile_model3[2,1])) / mean(test_data$Churn)

#lift curve
pred_model3 <- prediction(predictions_model3, test_data$Churn)
perf_model3<- performance(pred_model3,"tpr","fpr")
plot(perf_model3,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model3 <- performance(pred_model3,"auc")

#Gini
as.numeric(auc_model3@y.values)*2-1


## AIC Both model ----
Logistic_regression_both <- stepAIC(Logistic_regression_full, direction="both", trace = TRUE)

### AIC both model validation criteria ----
# Fit Criteria
predictions_model4 <- predict(Logistic_regression_both, type = "response", newdata=test_data)
comparison_df$prediction4 <- predictions_model4

#Hit Rate Table
predicted_model4 <- ifelse(predictions_model4>.5,1,0)
hit_rate_model4 <- table(test_data$Churn, predicted_model4, dnn= c("Observed", "Predicted"))
hit_rate_model4
(hit_rate_model4[1,1]+hit_rate_model4[2,2])/sum(hit_rate_model4)

#Decile lift
decile_predicted_model4 <- ntile(-predictions_model4, 10)
decile_model4 <- table(test_data$Churn, decile_predicted_model4, dnn= c("Observed", "Decile"))
decile_model4

(decile_model4[2,1] / (decile_model4[1,1]+ decile_model4[2,1])) / mean(test_data$Churn)

#lift curve
pred_model4 <- prediction(predictions_model4, test_data$Churn)
perf_model4<- performance(pred_model4,"tpr","fpr")
plot(perf_model4,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model4 <- performance(pred_model4,"auc")

#Gini
as.numeric(auc_model4@y.values)*2-1

## BIC backward ----
Logistic_regression_backward_BIC <- stepAIC(Logistic_regression_full, direction="backward", trace = TRUE, k = log(nrow(training_data)))

### BIC Backward model validation criteria ----
# Fit Criteria
predictions_model5 <- predict(Logistic_regression_backward_BIC, type = "response", newdata=test_data)
comparison_df$prediction5 <- predictions_model5

#Hit Rate Table
predicted_model5 <- ifelse(predictions_model5>.5,1,0)
hit_rate_model5 <- table(test_data$Churn, predicted_model5, dnn= c("Observed", "Predicted"))
hit_rate_model5
(hit_rate_model5[1,1]+hit_rate_model5[2,2])/sum(hit_rate_model5)

#Decile lift
decile_predicted_model5 <- ntile(-predictions_model5, 10)
decile_model5 <- table(test_data$Churn, decile_predicted_model5, dnn= c("Observed", "Decile"))
decile_model5

(decile_model5[2,1] / (decile_model5[1,1]+ decile_model5[2,1])) / mean(test_data$Churn)

#lift curve
pred_model5 <- prediction(predictions_model5, test_data$Churn)
perf_model5<- performance(pred_model5,"tpr","fpr")
plot(perf_model5,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model5 <- performance(pred_model5,"auc")

#Gini
as.numeric(auc_model5@y.values)*2-1

## BIC forward ----
Logistic_regression_forward_BIC <- stepAIC(Logistic_regression_null, direction="forward", scope=list(lower=Logistic_regression_null, upper=Logistic_regression_full), trace = TRUE, k = log(nrow(training_data)))

### BIC Forward model validation criteria ----
# Fit Criteria
predictions_model6 <- predict(Logistic_regression_forward_BIC, type = "response", newdata=test_data)
comparison_df$prediction6 <- predictions_model6

#Hit Rate Table
predicted_model6 <- ifelse(predictions_model6>.5,1,0)
hit_rate_model6 <- table(test_data$Churn, predicted_model6, dnn= c("Observed", "Predicted"))
hit_rate_model6
(hit_rate_model6[1,1]+hit_rate_model6[2,2])/sum(hit_rate_model6)

#Decile lift
decile_predicted_model6 <- ntile(-predictions_model6, 10)
decile_model6 <- table(test_data$Churn, decile_predicted_model6, dnn= c("Observed", "Decile"))
decile_model6

(decile_model6[2,1] / (decile_model6[1,1]+ decile_model6[2,1])) / mean(test_data$Churn)

#lift curve
pred_model6 <- prediction(predictions_model6, test_data$Churn)
perf_model6<- performance(pred_model6,"tpr","fpr")
plot(perf_model6,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model6 <- performance(pred_model6,"auc")

#Gini
as.numeric(auc_model6@y.values)*2-1

## BIC both ----
Logistic_regression_both <- stepAIC(Logistic_regression_full, direction="both", trace = TRUE, k = log(nrow(training_data)))

### BIC both model validation criteria ----
# Fit Criteria
predictions_model7 <- predict(Logistic_regression_forward_BIC, type = "response", newdata=test_data)
comparison_df$prediction7 <- predictions_model7

#Hit Rate Table
predicted_model7 <- ifelse(predictions_model7>.5,1,0)
hit_rate_model7 <- table(test_data$Churn, predicted_model7, dnn= c("Observed", "Predicted"))
hit_rate_model7
(hit_rate_model7[1,1]+hit_rate_model7[2,2])/sum(hit_rate_model7)

#Decile lift
decile_predicted_model7 <- ntile(-predictions_model7, 10)
decile_model7 <- table(test_data$Churn, decile_predicted_model7, dnn= c("Observed", "Decile"))
decile_model7

(decile_model7[2,1] / (decile_model7[1,1]+ decile_model7[2,1])) / mean(test_data$Churn)

#lift curve
pred_model7 <- prediction(predictions_model7, test_data$Churn)
perf_model7<- performance(pred_model7,"tpr","fpr")
plot(perf_model7,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model7 <- performance(pred_model7,"auc")

#Gini
as.numeric(auc_model7@y.values)*2-1

## CART Model ----
Cart_tree1 <- rpart(Churn ~ ., data = training_data, method = "class")
Cart_tree1_visual <- as.party(Cart_tree1)
plot(Cart_tree1_visual , type="simple", gp = gpar(fontsize = 10))

newsettings1 <- rpart.control(minsplit = 100, minbucket = 50, cp = 0.01, maxdepth = 3)

Cart_tree2 <- rpart(Churn ~ ., data = training_data, method = "class", control = "newsettings1")
Cart_tree2_visual <- as.party(Cart_tree2)
plot(Cart_tree2_visual , type="simple", gp = gpar(fontsize = 10))
# setting change does not change result or number of splits



## CART Model validation criteria ----
# Fit Criteria
predictions_model8 <- predict(Cart_tree1, type = "class", newdata = test_data)
comparison_df$prediction_cart <- predictions_model8

#Hit Rate Table
probabilities_model8 <- predict(Cart_tree1, type = "prob", newdata = test_data)
predicted_model8 <- ifelse(probabilities_model8[, 2] > 0.5, 1, 0)  # Convert probabilities to binary predictions
hit_rate_model8 <- table(test_data$Churn, predicted_model8, dnn= c("Observed", "Predicted"))
hit_rate_model8
(hit_rate_model8[1,1]+hit_rate_model8[2,2])/sum(hit_rate_model8)

#Decile lift
predictions_model8 <- predict(Cart_tree1, type = "prob", newdata = test_data)[, 2]
decile_predicted_model8 <- ntile(-predictions_model8, 10)
decile_model8 <- table(test_data$Churn, decile_predicted_model8, dnn= c("Observed", "Decile"))
decile_model8

(decile_model8[2,1] / (decile_model8[1,1]+ decile_model8[2,1])) / mean(test_data$Churn)

#lift curve
pred_model8 <- prediction(predictions_model8, test_data$Churn)
perf_model8<- performance(pred_model8,"tpr","fpr")
plot(perf_model8,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model8 <- performance(pred_model8,"auc")

#Gini
as.numeric(auc_model8@y.values)*2-1


## CHAID Model ----
CHAID_tree1 <- ctree(Churn ~ ., data = training_data,
                     control = ctree_control(minsplit = 100, minbucket = 50, maxdepth = 3))

# Visualize the first tree
plot(CHAID_tree1, type = "simple")

# Adjust control parameters for a second tree
newsettings2 <- ctree_control(minsplit = 200, minbucket = 100, maxdepth = 2)
CHAID_tree2 <- ctree(Churn ~ ., data = training_data, control = newsettings2)

# Visualize the second tree
plot(CHAID_tree2, type = "simple")

# Predict probabilities for test data
predictions_chaid1 <- predict(CHAID_tree1, newdata = test_data, type = "prob")
predictions_chaid2 <- predict(CHAID_tree2, newdata = test_data, type = "prob")

# Add the predicted probabilities for the positive class to the test data
comparison_df$prediction_chaid <- predictions_chaid1

## CHAID model validation criteria ----

### Tree 1 ----
# Fit Criteria
predictions_model9 <- predict(CHAID_tree1, type = "response", newdata = test_data)
comparison_df$prediction_chaid <- predictions_model9

#Hit Rate Table
probabilities_model9 <- predict(CHAID_tree1, type = "prob", newdata = test_data)
probabilities_model9 <- sapply(probabilities_model9, function(ecdf_fn) ecdf_fn(0.5))

predicted_model9 <- ifelse(probabilities_model9 > 0.5, 1, 0)  # Convert probabilities to binary predictions
hit_rate_model9 <- table(test_data$Churn, predicted_model9, dnn= c("Observed", "Predicted"))
hit_rate_model9
(hit_rate_model9[1,1]+hit_rate_model9[2,2])/sum(hit_rate_model9)

#Decile lift
predictions_model9 <- predict(CHAID_tree1, type = "response", newdata = test_data)
decile_predicted_model9 <- ntile(-predictions_model9, 10)
decile_model9 <- table(test_data$Churn, decile_predicted_model9, dnn= c("Observed", "Decile"))
decile_model9

(decile_model9[2,1] / (decile_model9[1,1]+ decile_model9[2,1])) / mean(test_data$Churn)

#lift curve
pred_model9 <- prediction(predictions_model9, test_data$Churn)
perf_model9<- performance(pred_model9,"tpr","fpr")
plot(perf_model9,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model9 <- performance(pred_model9,"auc")

#Gini
as.numeric(auc_model9@y.values)*2-1

### Tree 2 ----
# Fit Criteria
predictions_model9 <- predict(CHAID_tree2, type = "response", newdata = test_data)
comparison_df$prediction_chaid <- predictions_model9

#Hit Rate Table
probabilities_model9 <- predict(CHAID_tree2, type = "prob", newdata = test_data)
probabilities_model9 <- sapply(probabilities_model9, function(ecdf_fn) ecdf_fn(0.5))

predicted_model9 <- ifelse(probabilities_model9 > 0.5, 1, 0)  # Convert probabilities to binary predictions
hit_rate_model9 <- table(test_data$Churn, predicted_model9, dnn= c("Observed", "Predicted"))
hit_rate_model9
(hit_rate_model9[1,1]+hit_rate_model9[2,2])/sum(hit_rate_model9)

#Decile lift
predictions_model9 <- predict(CHAID_tree2, type = "response", newdata = test_data)
decile_predicted_model9 <- ntile(-predictions_model9, 10)
decile_model9 <- table(test_data$Churn, decile_predicted_model9, dnn= c("Observed", "Decile"))
decile_model9

(decile_model9[2,1] / (decile_model9[1,1]+ decile_model9[2,1])) / mean(test_data$Churn)

#lift curve
pred_model9 <- prediction(predictions_model9, test_data$Churn)
perf_model9<- performance(pred_model9,"tpr","fpr")
plot(perf_model9,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model9 <- performance(pred_model9,"auc")

#Gini
as.numeric(auc_model9@y.values)*2-1

## Bagging ----
newsettings2 <- rpart.control(minsplit = 2, cp = 0.0)
Bagging_tree1 <- bagging(Churn ~ ., data=training_data, method="treebag", nbagg=500, coob=TRUE, control=newsettings2)
predictions_bagging1 <- predict(Bagging_tree1, newdata = test_data, type = "prob")
comparison_df$bagging <- predictions_bagging1

## Bagging validation criteria----
# Fit Criteria
predictions_model10 <- predict(Bagging_tree1, type = "response", newdata=test_data)
comparison_df$prediction10 <- predictions_model10

#Hit Rate Table
predicted_model10 <- ifelse(predictions_model10>.5,1,0)
hit_rate_model10 <- table(test_data$Churn, predicted_model10, dnn= c("Observed", "Predicted"))
hit_rate_model10
(hit_rate_model10[1,1]+hit_rate_model10[2,2])/sum(hit_rate_model10)

#Decile lift
decile_predicted_model10 <- ntile(-predictions_model10, 10)
decile_model10 <- table(test_data$Churn, decile_predicted_model10, dnn= c("Observed", "Decile"))
decile_model10

(decile_model10[2,1] / (decile_model10[1,1]+ decile_model10[2,1])) / mean(test_data$Churn)

#lift curve
pred_model10 <- prediction(predictions_model10, test_data$Churn)
perf_model10<- performance(pred_model10,"tpr","fpr")
plot(perf_model10,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model10 <- performance(pred_model10,"auc")

#Gini
as.numeric(auc_model10@y.values)*2-1

## Boosting ----
boost_tree1 <- gbm(Churn ~ ., data=training_data, distribution = "bernoulli", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
boost_tree1

## Boosting validation criteria ----
# Fit Criteria
predictions_model11 <- predict(boost_tree1, type = "response", newdata=test_data)
comparison_df$prediction11 <- predictions_model11

#Hit Rate Table
predicted_model11 <- ifelse(predictions_model11>.5,1,0)
hit_rate_model11 <- table(test_data$Churn, predicted_model11, dnn= c("Observed", "Predicted"))
hit_rate_model11
(hit_rate_model11[1,1]+hit_rate_model11[2,2])/sum(hit_rate_model11)

#Decile lift
decile_predicted_model11 <- ntile(-predictions_model11, 10)
decile_model11 <- table(test_data$Churn, decile_predicted_model11, dnn= c("Observed", "Decile"))
decile_model11

(decile_model11[2,1] / (decile_model11[1,1]+ decile_model11[2,1])) / mean(test_data$Churn)

#lift curve
pred_model11 <- prediction(predictions_model11, test_data$Churn)
perf_model11<- performance(pred_model11,"tpr","fpr")
plot(perf_model11,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model11 <- performance(pred_model11,"auc")

#Gini
as.numeric(auc_model11@y.values)*2-1

## Random Forest ----
Random_forest1 <- randomForest(as.factor(Churn) ~ ., data=training_data, importance=TRUE)

predictions_forest1 <- predict(Random_forest1, newdata=test_data, type ="prob")

varImpPlot(Random_forest1)

#Some extra setting you can play around with
Random_forest1 <- randomForest(as.factor(Churn) ~ ., data=training_data, 
                               ntree=10000, mtry=3, nodesize=1, maxnodes=100, importance=TRUE)

## Random forest validation criteria ----
# Fit Criteria
predictions_model12 <- predict(Random_forest1, type = "prob", newdata=test_data)[, 2]
comparison_df$prediction12 <- predictions_model12

#Hit Rate Table
predicted_model12 <- ifelse(predictions_model12>.5,1,0)
hit_rate_model12 <- table(test_data$Churn, predicted_model12, dnn= c("Observed", "Predicted"))
hit_rate_model12
(hit_rate_model12[1,1]+hit_rate_model12[2,2])/sum(hit_rate_model12)

#Decile lift
decile_predicted_model12 <- ntile(-predictions_model12, 10)
decile_model12 <- table(test_data$Churn, decile_predicted_model12, dnn= c("Observed", "Decile"))
decile_model12

(decile_model12[2,1] / (decile_model12[1,1]+ decile_model12[2,1])) / mean(test_data$Churn)

#lift curve
pred_model12 <- prediction(predictions_model12, test_data$Churn)
perf_model12<- performance(pred_model12,"tpr","fpr")
plot(perf_model12,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model12 <- performance(pred_model12,"auc")

# gini
as.numeric(auc_model12@y.values)*2-1

## Support Vector machine ----
svm_13 <- svm(Churn ~ Electricity_usage + Contract_length, data = training_data, type = 'C-classification',
              probability = TRUE, kernel = 'linear')
plot(svm_13, training_data, Contract_length~Electricity_usage)

svm_14 <- svm(Churn ~ Electricity_usage + Contract_length, data = training_data, type = 'C-classification',
              probability = TRUE, kernel = 'polynomial')
plot(svm_14, training_data, Contract_length~Electricity_usage)

svm_15 <- svm(Churn ~ Electricity_usage + Contract_length, data = training_data, type = 'C-classification',
              probability = TRUE, kernel = 'radial')
plot(svm_15, training_data, Contract_length~Electricity_usage)

svm_16 <- svm(Churn ~ Electricity_usage + Contract_length, data = training_data, type = 'C-classification',
              probability = TRUE, kernel = 'polynomial', degree = 2)
plot(svm_16, training_data, Contract_length ~ Electricity_usage)

svm_17 <- svm(Churn ~ Electricity_usage + Contract_length, data = training_data, type = 'C-classification',
              probability = TRUE, kernel = 'sigmoid')
plot(svm_17, training_data, Contract_length ~ Electricity_usage)


## Vector validation criteria
# Fit Criteria
predictions_model13 <- predict(svm_13, type = "response", newdata=test_data)
comparison_df$prediction13 <- predictions_model13

#Hit Rate Table
predictions_model13 <- as.numeric(as.character(predictions_model13))

predicted_model13 <- ifelse(predictions_model13>.5,1,0)
hit_rate_model13 <- table(test_data$Churn, predicted_model13, dnn= c("Observed", "Predicted"))
hit_rate_model13
(hit_rate_model13[1,1]+hit_rate_model13[2,2])/sum(hit_rate_model13)

#Decile lift
decile_predicted_model13 <- ntile(-predictions_model13, 10)
decile_model13 <- table(test_data$Churn, decile_predicted_model13, dnn= c("Observed", "Decile"))
decile_model13

(decile_model13[2,1] / (decile_model13[1,1]+ decile_model13[2,1])) / mean(test_data$Churn)

#lift curve
pred_model13 <- prediction(predictions_model13, test_data$Churn)
perf_model13<- performance(pred_model13,"tpr","fpr")
plot(perf_model13,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model13 <- performance(pred_model13,"auc")

# gini
as.numeric(auc_model13@y.values)*2-1

predictions_model14 <- predict(svm_14, type = "response", newdata=test_data)
comparison_df$prediction14 <- predictions_model14

#Hit Rate Table
predictions_model14 <- as.numeric(as.character(predictions_model14))

predicted_model14 <- ifelse(predictions_model14>.5,1,0)
hit_rate_model14 <- table(test_data$Churn, predicted_model14, dnn= c("Observed", "Predicted"))
hit_rate_model14
(hit_rate_model14[1,1]+hit_rate_model14[2,2])/sum(hit_rate_model14)

#Decile lift
decile_predicted_model14 <- ntile(-predictions_model14, 10)
decile_model14 <- table(test_data$Churn, decile_predicted_model14, dnn= c("Observed", "Decile"))
decile_model14

(decile_model14[2,1] / (decile_model14[1,1]+ decile_model14[2,1])) / mean(test_data$Churn)

#lift curve
pred_model14 <- prediction(predictions_model14, test_data$Churn)
perf_model14<- performance(pred_model14,"tpr","fpr")
plot(perf_model14,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model14 <- performance(pred_model14,"auc")

# gini
as.numeric(auc_model14@y.values)*2-1

predictions_model15 <- predict(svm_15, type = "response", newdata=test_data)
comparison_df$prediction15 <- predictions_model15

#Hit Rate Table
predictions_model15 <- as.numeric(as.character(predictions_model15))

predicted_model15 <- ifelse(predictions_model15>.5,1,0)
hit_rate_model15 <- table(test_data$Churn, predicted_model15, dnn= c("Observed", "Predicted"))
hit_rate_model15
(hit_rate_model15[1,1]+hit_rate_model15[2,2])/sum(hit_rate_model15)

#Decile lift
decile_predicted_model15 <- ntile(-predictions_model15, 10)
decile_model15 <- table(test_data$Churn, decile_predicted_model15, dnn= c("Observed", "Decile"))
decile_model15

(decile_model15[2,1] / (decile_model15[1,1]+ decile_model15[2,1])) / mean(test_data$Churn)

#lift curve
pred_model15 <- prediction(predictions_model15, test_data$Churn)
perf_model15<- performance(pred_model15,"tpr","fpr")
plot(perf_model15,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model15 <- performance(pred_model15,"auc")

# gini
as.numeric(auc_model15@y.values)*2-1

predictions_model16 <- predict(svm_16, type = "response", newdata=test_data)
comparison_df$prediction16 <- predictions_model16

#Hit Rate Table
predictions_model16 <- as.numeric(as.character(predictions_model16))

predicted_model16 <- ifelse(predictions_model16>.5,1,0)
hit_rate_model16 <- table(test_data$Churn, predicted_model16, dnn= c("Observed", "Predicted"))
hit_rate_model16
(hit_rate_model16[1,1]+hit_rate_model16[2,2])/sum(hit_rate_model16)

#Decile lift
decile_predicted_model16 <- ntile(-predictions_model16, 10)
decile_model16 <- table(test_data$Churn, decile_predicted_model16, dnn= c("Observed", "Decile"))
decile_model16

(decile_model16[2,1] / (decile_model16[1,1]+ decile_model16[2,1])) / mean(test_data$Churn)

#lift curve
pred_model16 <- prediction(predictions_model16, test_data$Churn)
perf_model16<- performance(pred_model16,"tpr","fpr")
plot(perf_model16,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model16 <- performance(pred_model16,"auc")

# gini
as.numeric(auc_model16@y.values)*2-1

predictions_model17 <- predict(svm_17, type = "response", newdata=test_data)
comparison_df$prediction17 <- predictions_model17

#Hit Rate Table
predictions_model17 <- as.numeric(as.character(predictions_model17))

predicted_model17 <- ifelse(predictions_model17>.5,1,0)
hit_rate_model17 <- table(test_data$Churn, predicted_model17, dnn= c("Observed", "Predicted"))
hit_rate_model17
(hit_rate_model17[1,1]+hit_rate_model17[2,2])/sum(hit_rate_model17)

#Decile lift
decile_predicted_model17 <- ntile(-predictions_model17, 10)
decile_model17 <- table(test_data$Churn, decile_predicted_model17, dnn= c("Observed", "Decile"))
decile_model17

(decile_model17[2,1] / (decile_model17[1,1]+ decile_model17[2,1])) / mean(test_data$Churn)

#lift curve
pred_model17 <- prediction(predictions_model17, test_data$Churn)
perf_model17<- performance(pred_model17,"tpr","fpr")
plot(perf_model17,xlab="Cumulative % of observations",ylab="Cumulative % of positive cases",xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")
auc_model17 <- performance(pred_model17,"auc")

# gini
as.numeric(auc_model17@y.values)*2-1
