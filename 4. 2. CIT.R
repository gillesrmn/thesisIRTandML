library(dplyr)
library(caret)
library(caTools)
library(partykit)
library(randomForest)

set.seed(123)

bestmtry <- tuneRF(x = predictors_03, 
                   y = target_03, 
                   stepFactor = 1.2, 
                   improve = 0.01, 
                   trace = TRUE, 
                   plot = TRUE)

bestmtry

# Train the conditional inference tree
ct_03 <- cforest(
  total_score ~ .,
  data = train_03,
  ntree = 500,
  mtry = 7,
  control   = ctree_control(
    mincriterion = 0.95,
    minsplit = 20,
    minbucket = 7,
    maxdepth = 70)
)

# Predict on test set
pred_ct_03 <- predict(ct_03, newdata = test_03)

# Evaluate: MSE, RMSE, R-squared
mse_ct_03  <- mean((test_03$total_score - pred_ct_03)^2)
rmse_ct_03 <- sqrt(mse_ct_03)

sst_ct_03  <- sum((test_03$total_score - mean(test_03$total_score))^2)
sse_ct_03  <- sum((test_03$total_score - pred_ct_03)^2)
r2_ct_03   <- 1 - (sse_ct_03 / sst_ct_03)

cat("Test RMSE:", round(rmse_ct_03, 4), "\n")
cat("Test R-squared:", round(r2_ct_03, 4), "\n")

# Extract which items the tree actually used (selected nodes)
ct_03_varimp <- varimp(ct_03)
ct_03_items  <- names(ct_03_varimp[ct_03_varimp > 0])
ct_03_items <- ct_03_items[ct_03_items != "total_score"]

# 13 most important
# Rank all items by importance and keep the top 13
ct_03_varimp_sorted <- sort(ct_03_varimp, decreasing = TRUE)
top13_names  <- names(ct_03_varimp_sorted)[1:13]
top13_scores <- ct_03_varimp_sorted[1:13]

# Print ranked table
top13_df <- data.frame(
  Rank       = 1:13,
  Item       = top13_names,
  Importance = round(as.numeric(top13_scores), 4)
)
print(top13_df, row.names = FALSE)
write.table(top13_df,"top13_03.txt",row.names = FALSE, sep = "\t")

# Bar chart of the top 13
par(mar = c(5, 5, 4, 2))
barplot(
  rev(top13_scores),
  names.arg = rev(top13_names),
  horiz     = TRUE,
  las       = 1,
  col       = colorRampPalette(c("steelblue1", "steelblue4"))(13),
  xlab      = "Variable Importance",
  main      = "Top 13 Most Important EI Items — Dataset 03",
  border    = NA
)







set.seed(123)

bestmtry <- tuneRF(x = predictors_04, 
                   y = target_04, 
                   stepFactor = 1.2, 
                   improve = 0.01, 
                   trace = TRUE, 
                   plot = TRUE)

bestmtry

# Train the conditional inference tree
ct_04 <- cforest(
  total_score ~ .,
  data = train_04,
  ntree = 500,
  mtry = 5,
  control   = ctree_control(
    mincriterion = 0.95,
    minsplit = 20,
    minbucket = 7,
    maxdepth = 70)
)

# Predict on test set
pred_ct_04 <- predict(ct_04, newdata = test_04)

# Evaluate: MSE, RMSE, R-squared
mse_ct_04  <- mean((test_04$total_score - pred_ct_04)^2)
rmse_ct_04 <- sqrt(mse_ct_04)

sst_ct_04  <- sum((test_04$total_score - mean(test_04$total_score))^2)
sse_ct_04  <- sum((test_04$total_score - pred_ct_04)^2)
r2_ct_04   <- 1 - (sse_ct_04 / sst_ct_04)

cat("Test RMSE:", round(rmse_ct_04, 4), "\n")
cat("Test R-squared:", round(r2_ct_04, 4), "\n")

# Extract which items the tree actually used (selected nodes)
ct_04_varimp <- varimp(ct_04)
ct_04_items  <- names(ct_04_varimp[ct_04_varimp > 0])
ct_04_items <- ct_04_items[ct_04_items != "total_score"]

# 9 most important
# Rank all items by importance and keep the top 9
ct_04_varimp_sorted <- sort(ct_04_varimp, decreasing = TRUE)
top9_names  <- names(ct_04_varimp_sorted)[1:9]
top9_scores <- ct_04_varimp_sorted[1:9]

# Print ranked table
top9_df <- data.frame(
  Rank       = 1:9,
  Item       = top9_names,
  Importance = round(as.numeric(top9_scores), 4)
)
print(top9_df, row.names = FALSE)
write.table(top9_df,"top9_04.txt",row.names = FALSE, sep = "\t")

# Bar chart of the top 9
par(mar = c(5, 5, 4, 2))
barplot(
  rev(top9_scores),
  names.arg = rev(top9_names),
  horiz     = TRUE,
  las       = 1,
  col       = colorRampPalette(c("steelblue1", "steelblue4"))(9),
  xlab      = "Variable Importance",
  main      = "Top 9 Most Important EI Items — Dataset 04",
  border    = NA
)

