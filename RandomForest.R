library(dplyr)
library(randomForest)
library(caret)

set.seed(123)

#LOAD DATASETS -----------------------------------------------------------------
#train_03 <- read.csv("EI_25_train.csv")
train_03 <- EI_25_train
#test_03 <- read.csv("EI_25_val.csv")
test_03 <- EI_25_val
#train_04 <- read.csv("EI_17_train.csv")
train_04 <- EI_17_train
#test_04 <- read.csv("EI_17_val.csv")
test_04 <- EI_17_val

#Create total_score column
train_03$total_score <- rowMeans(train_03, na.rm = TRUE)
test_03$total_score <- rowMeans(test_03,  na.rm = TRUE)
train_04$total_score <- rowMeans(train_04, na.rm = TRUE)
test_04$total_score <- rowMeans(test_04,  na.rm = TRUE)

#Split predictor & target variables
predictors_03 <- train_03 %>% select(-total_score)
target_03 <- train_03$total_score
predictors_04 <- train_04 %>% select(-total_score)
target_04 <- train_04$total_score


#START RANDOM FOREST ANALYSIS --------------------------------------------------
bestmtry <- tuneRF(x = predictors_03, 
                   y = target_03, 
                   stepFactor = 1.2, 
                   improve = 0.01, 
                   trace = TRUE, 
                   plot = TRUE)

bestmtry

# Train the random forest model
rf_03 <- randomForest(
  total_score ~ .,
  data = train_03,
  ntree = 500, #started at 500, but the plot suggested to move to 150 or even lower, but the % var explained started to decrease, so i left it at 500
  importance = TRUE,
  mtry = 7,
  replace = TRUE
)

print(rf_03)

varImpPlot(rf_03)
plot(rf_03, main = "Error Rate vs. Number of Trees")

# Predict on the test set
pred_03 <- predict(rf_03, newdata = test_03)

# Evaluate the model
# 2. Calculate Mean Squared Error (MSE) and RMSE
mse_test <- mean((test_03$total_score - pred_03)^2)
rmse_test <- sqrt(mse_test)

# 3. Calculate Test R-squared (Variance Explained)
sst <- sum((test_03$total_score - mean(test_03$total_score))^2)
sse <- sum((test_03$total_score - pred_03)^2)
r2_test <- 1 - (sse / sst)

# 4. Print the results
cat("Test RMSE:", rmse_test, "\n")
cat("Test R-squared:", r2_test, "\n")

# View the importance of each item
imp_03 <- as.data.frame(importance(rf_03, type = 1))
imp_03$item <- rownames(imp_03)
imp_03 <- as.data.frame(imp_03[order(-imp_03$`%IncMSE`), ])
imp_03
write.table(imp_03,"importance03.txt",row.names = FALSE, sep = "\t")
imp_03 <- imp_03$item[1:13] #Select top 13 items based on importance
print(imp_03)

# Standard Plot for publications
varImpPlot(rf_03, main = "Feature Importance: Predictors of Total Score")













bestmtry <- tuneRF(x = predictors_04, 
                   y = target_04, 
                   stepFactor = 1.2, 
                   improve = 0.01, 
                   trace = TRUE, 
                   plot = TRUE)

bestmtry

# Train the random forest model
rf_04 <- randomForest(
  total_score ~ .,
  data = train_04,
  ntree = 500, #started at 500, but the plot suggested to move to 150 or even lower, but the % var explained started to decrease, so i left it at 500
  importance = TRUE,
  mtry = 5,
  replace = TRUE
)

print(rf_04)

varImpPlot(rf_04)
plot(rf_04, main = "Error Rate vs. Number of Trees")

# Predict on the test set
pred_04 <- predict(rf_04, newdata = test_04)

# Evaluate the model
# 2. Calculate Mean Squared Error (MSE) and RMSE
mse_test <- mean((test_04$total_score - pred_04)^2)
rmse_test <- sqrt(mse_test)

# 3. Calculate Test R-squared (Variance Explained)
sst <- sum((test_04$total_score - mean(test_04$total_score))^2)
sse <- sum((test_04$total_score - pred_04)^2)
r2_test <- 1 - (sse / sst)

# 4. Print the results
cat("Test RMSE:", rmse_test, "\n")
cat("Test R-squared:", r2_test, "\n")

# View the importance of each item
imp_04 <- as.data.frame(importance(rf_04, type = 1))
imp_04$item <- rownames(imp_04)
imp_04 <- as.data.frame(imp_04[order(-imp_04$`%IncMSE`), ])
imp_04
write.table(imp_04,"importance04.txt",row.names = FALSE, sep = "\t")
imp_04 <- imp_04$item[1:9] #Select top 12 items based on importance
print(imp_04)

# Standard Plot for publications
varImpPlot(rf_04, main = "Feature Importance: Predictors of Total Score")

