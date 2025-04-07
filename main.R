# Install and load necessary libraries
install.packages("tidyverse")  # Data manipulation
install.packages("caTools")    # Splitting dataset
install.packages("caret")      # Model evaluation
install.packages("pROC")       # ROC Curve for evaluation
library(tidyverse)
library(caTools)
library(caret)
library(pROC)
library(csv)
# The 'csv' package is not a standard R package and is likely unnecessary. 
# read.csv() is part of the base 'utils' package and is loaded by default.
# Removing library(csv)
# Load the dataset
heart=read.csv(file.choose(),header = T)

# View the first few rows
head(heart)

# Check for missing values
sum(is.na(heart))

# Convert categorical variables to factors
heart$sex <- as.factor(heart$sex)
heart$cp <- as.factor(heart$cp)
heart$fbs <- as.factor(heart$fbs)
heart$restecg <- as.factor(heart$restecg)
heart$exang <- as.factor(heart$exang)
heart$slope <- as.factor(heart$slope)
heart$ca <- as.factor(heart$ca)
heart$thal <- as.factor(heart$thal)

# Define the target variable (1 = Heart Disease, 0 = No Heart Disease)
heart$target <- as.factor(heart$target)

# Split the dataset into training (80%) and testing (20%)
set.seed(123)
split <- sample.split(heart$target, SplitRatio = 0.8)
train_data <- subset(heart, split == TRUE)
test_data <- subset(heart, split == FALSE)

# Build a Logistic Regression Model
log_model <- glm(target ~ ., data = train_data, family = binomial)

# Summary of the model
summary(log_model)

# Make predictions on test set
test_pred <- predict(log_model, test_data, type = "response")

# Convert probabilities to binary classification (threshold = 0.5)
test_pred_class <- ifelse(test_pred > 0.5, 1, 0)

# Model Evaluation
conf_matrix <- table(Predicted = test_pred_class, Actual = test_data$target)
print("Confusion Matrix:")
print(conf_matrix)

# Compute Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# ROC Curve and AUC
roc_curve <- roc(test_data$target, test_pred)
plot(roc_curve, col = "blue", main = "ROC Curve for Heart Disease Prediction")
auc(roc_curve)

