## --------- FINAL DEPLOYMENT VERSION -------------
# Giacomo Fantato ---- K-16958 - g.fantato@student.uw.edu.pl
setwd("C:\\UW\\Second Semester\\Social Networks in Economics Geo\\Project - OLS vs ML")
library(sf)
library(spdep)
library(sp)
library(ggplot2)
library(haven)
library(tidyverse)
library(metrica)
library(dplyr)
library(purrr)
library(tidyr)
library(oce)
library(Metrics)
library(lmtest)
library(nortest)
library(xgboost)
library(caret)
library(pROC)

map <- st_read("#housing data/lucas-county-ohio-census-tracts.shp") %>% 
  st_transform(crs = 4269)
data.file <- read_dta("#housing data/LucasCountytemp2-NN50.dta") %>% as.data.frame()
data.pcg <- house  
data.pcg$id <- 1:nrow(data.pcg)
data.sf <- st_as_sf(data.pcg, coords = c("long", "lat"), crs = 2834) %>% 
  st_transform(crs = 4269)
SAFE <- st_join(data.sf, map, join = st_intersects)
data.pcg$SAFE <- as.factor(SAFE$TRACTCE10)

data <- merge(data.pcg, data.file, by = "id")
data.sf <- st_as_sf(data, coords = c("long", "lat"), crs = 2834) %>% 
  st_transform(crs = 4269)
data.train <- data[data$syear != "1998", ]
data.test <- data[data$syear == "1998", ]

data.train.sf <- st_as_sf(data.train, coords = c("long", "lat"), crs = 2834) %>% 
  st_transform(crs = 4269)
data.test.sf <- st_as_sf(data.test, coords = c("long", "lat"), crs = 2834) %>% 
  st_transform(crs = 4269)
coords.train <- st_coordinates(data.train.sf)
coords.test <- st_coordinates(data.test.sf)

data.train$latitude <- coords.train[, 2]
data.train$longitude <- coords.train[, 1]
data.test$latitude <- coords.test[, 2]
data.test$longitude <- coords.test[, 1]
cols_needed <- c("log_price", "log_age", "log_lotsize", "log_livearea", "Story2more",
                 "wall", "beds", "baths", "dGarage", "dToledo", "MeanPrice", 
                 "XWX2M", "XWX3M", "XWX4M", "XWX6M", "XWX7M", "XWX8M", "SAFE", "latitude", "longitude")

data.train.df <- as.data.frame(data.train)
data.train.clean <- data.train.df[complete.cases(data.train.df), ]
train_matrix <- model.matrix(~ log_age + log_lotsize + log_livearea + Story2more +
                               wall + beds + baths + dGarage + dToledo + MeanPrice + 
                               XWX2M + XWX3M + XWX4M + XWX6M + XWX7M + XWX8M + SAFE - 1, 
                             data = data.train.clean)
train_matrix <- cbind(train_matrix, latitude = data.train.clean$latitude, longitude = data.train.clean$longitude)

train_label <- data.train.clean$log_price
test_matrix <- model.matrix(~ log_age + log_lotsize + log_livearea + Story2more +
                              wall + beds + baths + dGarage + dToledo + MeanPrice + 
                              XWX2M + XWX3M + XWX4M + XWX6M + XWX7M + XWX8M + SAFE - 1, 
                            data = data.test)
test_matrix <- cbind(test_matrix, latitude = data.test$latitude, longitude = data.test$longitude)

#XGBoost matrix creation
xgb_train <- xgb.DMatrix(data = as.matrix(train_matrix), label = train_label)
xgb_test <- xgb.DMatrix(data = as.matrix(test_matrix))

# Training:
#params <- list(
#  objective = "reg:squarederror",
 # eval_metric = "rmse",
  #eta = 0.1,
  #max_depth = 6,
  #subsample = 0.7,
  #colsample_bytree = 0.7
#)
# ----------- Final training ------------
params <- list(
  objective = "reg:squarederror",  # Use squared error loss for regression
  eval_metric = "rmse",  # Evaluate the model using Root Mean Squared Error (RMSE)
  eta = 0.05,  # Reduce the learning rate to make training more gradual
  max_depth = 4,  # Limit tree depth to prevent overfitting
  subsample = 0.6,  # Use 60% of the training data for each tree to improve generalization
  colsample_bytree = 0.6,  # Use 60% of the features for each tree to reduce correlation
  lambda = 1.0,  # Apply L2 regularization (Ridge) to prevent overfitting
  alpha = 0.5  # Apply L1 regularization (Lasso) to encourage feature sparsity
)

data.test.df <- as.data.frame(data.test)  #df conversion
data.test.clean <- data.test.df[complete.cases(data.test.df), ]  #NA cleaning 
test_matrix <- model.matrix(~ log_age + log_lotsize + log_livearea + Story2more +
                              wall + beds + baths + dGarage + dToledo + MeanPrice + 
                              XWX2M + XWX3M + XWX4M + XWX6M + XWX7M + XWX8M + SAFE - 1, 
                            data = data.test.clean)

test_matrix <- cbind(test_matrix, latitude = data.test.clean$latitude, longitude = data.test.clean$longitude)

xgb_model <- xgb.train(params = params, data = xgb_train, nrounds = 500, 
                       watchlist = list(train = xgb_train), verbose = 1)

# Predictions on the test set
predictions <- predict(xgb_model, xgb_test)
data.pred.XGB <- data.frame(obs = data.test$log_price, pred = predictions)
data.test.clean <- as.data.frame(data.test)  
data.test.clean <- data.test.clean[complete.cases(data.test.clean), ]
data.pred.XGB <- data.frame(obs = data.test.clean$log_price, pred = predictions)

#--------- overfitting test: CROSS VALIDATION -------------

cross_validation_test <- function(data, params, n_folds = 5, n_rounds = 500) {
  #K-FOLDS ALGO
    folds <- sample(rep(1:n_folds, length.out = nrow(data)))
  
  rmse_train_values <- c()
  rmse_test_values <- c()
  
  for (i in 1:n_folds) {
    cat("Running Fold", i, "...\n")
    
    # Separazione tra train e test
    train_data <- data[folds != i, ]
    test_data <- data[folds == i, ]
    
    # Creazione delle matrici di train e test
    train_matrix <- model.matrix(~ log_age + log_lotsize + log_livearea + Story2more +
                                   wall + beds + baths + dGarage + dToledo + MeanPrice + 
                                   XWX2M + XWX3M + XWX4M + XWX6M + XWX7M + XWX8M + SAFE - 1, 
                                 data = train_data)
    
    train_matrix <- cbind(train_matrix, latitude = train_data$latitude, longitude = train_data$longitude)
    train_label <- train_data$log_price
    
    test_matrix <- model.matrix(~ log_age + log_lotsize + log_livearea + Story2more +
                                  wall + beds + baths + dGarage + dToledo + MeanPrice + 
                                  XWX2M + XWX3M + XWX4M + XWX6M + XWX7M + XWX8M + SAFE - 1, 
                                data = test_data)
    
    test_matrix <- cbind(test_matrix, latitude = test_data$latitude, longitude = test_data$longitude)
    test_label <- test_data$log_price
    
    # Conversione in formato XGBoost
    xgb_train <- xgb.DMatrix(data = as.matrix(train_matrix), label = train_label)
    xgb_test <- xgb.DMatrix(data = as.matrix(test_matrix), label = test_label)
    
    # Training del modello
    xgb_model <- xgb.train(params = params, data = xgb_train, nrounds = n_rounds, verbose = 0)
    
    # Predizioni
    train_pred <- predict(xgb_model, xgb_train)
    test_pred <- predict(xgb_model, xgb_test)
    
    # Calcolo degli errori
    rmse_train <- sqrt(mean((train_pred - train_label)^2))
    rmse_test <- sqrt(mean((test_pred - test_label)^2))
    
    rmse_train_values <- c(rmse_train_values, rmse_train)
    rmse_test_values <- c(rmse_test_values, rmse_test)
  }
  
  rmse_train_mean <- mean(rmse_train_values)
  rmse_test_mean <- mean(rmse_test_values)
  
  cat("Average RMSE - Training Set:", rmse_train_mean, "\n")
  cat("Average RMSE - Test Set:", rmse_test_mean, "\n")
  
  overfit_ratio <- rmse_test_mean / rmse_train_mean
  cat("Overfitting Ratio (Test RMSE / Train RMSE):", overfit_ratio, "\n")
  
  if (overfit_ratio > 1.3) {
    cat("The model may be overfitting! Try reducing max_depth or increasing regularization.\n")
  } else {
    cat("The model appears to generalize well!\n")
  }
  
  return(list(rmse_train = rmse_train_mean, rmse_test = rmse_test_mean, overfit_ratio = overfit_ratio))
}
#ds cross validation
cv_results <- cross_validation_test(data.train.clean, params)

# -------------------------------------------------


# Importance matrix
importance_matrix <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix)
print(xgb_model$best_score)

# SAFE MAP with XGBoost
map$SAFE.ID <- paste0("SAFE", map$TRACTCE10)
importance_matrix <- xgb.importance(model = xgb_model)
print(importance_matrix)

coefs <- data.frame(name = importance_matrix$Feature, value = importance_matrix$Gain)
map <- merge(map, coefs, by.x="SAFE.ID", by.y="name", all.x=TRUE)
ggplot(data = map) +
  geom_sf(aes(fill = value)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "SAFE – Small Area Fixed Effects con XGBoost")



### ------------------------- ANALYSIS OF THE MODEL -------------------------------
# MAE and RMSE calculation for XGBoost
mae_XGB <- mae(data.pred.XGB$obs, data.pred.XGB$pred)
rmse_XGB <- rmse(data.pred.XGB$obs, data.pred.XGB$pred)
cat("MAE (XGBoost):", mae_XGB, "\n")
cat("RMSE (XGBoost):", rmse_XGB, "\n")

# Residuals analysis for XGBoost
residuals_XGB <- data.pred.XGB$obs - data.pred.XGB$pred

# 1) Histogram with Density Plot for XGBoost
ggplot(data.frame(residuals_XGB), aes(x = residuals_XGB)) +
  geom_histogram(aes(y = ..density.., fill = "Histogram"), bins = 30, color = "black", alpha = 0.7) + 
  geom_density(aes(color = "Density"), size = 1) +  
  scale_fill_manual(name = "Legend", values = c("Histogram" = "steelblue")) +  
  scale_color_manual(name = "Legend", values = c("Density" = "red")) + 
  labs(title = "Histogram of Residuals (XGBoost)", x = "Residuals", y = "Density") +
  theme_minimal() +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))

# 2) Q-Q Plot for XGBoost
ggplot(data.frame(residuals_XGB), aes(sample = residuals_XGB)) +
  stat_qq(aes(color = "Residuals"), size = 1) +  
  stat_qq_line(aes(color = "Theoretical Line"), size = 1, linetype = "solid") +  
  labs(title = "Q-Q Plot of Residuals (XGBoost)", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  scale_color_manual(name = "Legend", values = c("Residuals" = "blue", "Theoretical Line" = "red")) +  
  theme_minimal() +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))

# 3) Observed vs Residuals for XGBoost
ggplot(data = data.pred.XGB, aes(x = obs, y = residuals_XGB)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Distribution of Residuals (XGBoost)", x = "Observed Values", y = "Error (Residual)") +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))

# 4) Residuals vs Fitted for XGBoost
# Step 1: Get the Fitted Values for the Test Data
fitted_values_XGB_test <- predict(xgb_model, xgb_test)

# Step 2: Calculate Residuals for the Test Data
# Predicted values and observed values
data.pred.XGB <- data.frame(obs = data.test.clean$log_price, pred = fitted_values_XGB_test)

# Residuals: difference between observed and predicted values
residuals_XGB_test <- data.pred.XGB$obs - data.pred.XGB$pred

# Step 3: Create the Residuals vs Fitted Plot for Test Set
ggplot(data.frame(Fitted = fitted_values_XGB_test, Residuals = residuals_XGB_test), 
       aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values (XGBoost - Test Set)", x = "Fitted Values", y = "Residuals") +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))



# -------------  KS Test for normality of residuals
ks.test(residuals_XGB, "pnorm", mean(residuals_XGB), sd(residuals_XGB))

# ----------------- Anderson-Darling Test for normality
ad.test(residuals_XGB)

# -------------- Durbin-Watson Test for autocorrelation (XGBoost residuals)
dwtest(residuals_XGB ~ data.pred.XGB$pred)

# ------------- Breusch-Pagan Test for heteroscedasticity
bptest(residuals_XGB ~ data.pred.XGB$pred)

# --------- Feature importance for XGBoost
importance_matrix <- xgb.importance(model = xgb_model)
top_10_importance <- importance_matrix[1:10, ]
colors <- rep("lightblue", nrow(top_10_importance))
xgb.plot.importance(top_10_importance, 
                    main = "Top 10 Most Important Features (XGBoost)",
                    col = colors)
print(top_10_importance)

## ----------------- CONFUSION MATRIX -----------------
#conversion in classification
threshold <- median(data.test.clean$log_price) #based on median
data.test.clean$actual_class <- ifelse(data.test.clean$log_price >= threshold, 1, 0)
data.test.clean$pred_class <- ifelse(predictions >= threshold, 1, 0)
conf_matrix <- confusionMatrix(
  factor(data.test.clean$pred_class), 
  factor(data.test.clean$actual_class)
)
print(conf_matrix)

# -------- ROC CURVE ----------------
roc_curve <- roc(data.test.clean$actual_class, predictions)

ggplot(data = data.frame(TPR = roc_curve$sensitivities, 
                         FPR = 1 - roc_curve$specificities), 
       aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = paste("ROC Curve (AUC =", round(auc_value, 3), ")"),
       x = "False Positive Rate (1 - Specificity)",
       y = "True Positive Rate (Sensitivity)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")
