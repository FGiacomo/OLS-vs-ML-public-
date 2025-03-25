## --------- FINAL DEPLOYMENT VERSION -------------
# Giacomo Fantato ---- K-16958 - g.fantato@student.uw.edu.pl

setwd("C:\\UW\\Second Semester\\Social Networks in Economics Geo\\Project - OLS vs ML")
library(rpart)      # per il Decision Tree
library(rpart.plot)  # per visualizzare l'albero
library(sf)          # per analisi spaziali
library(spdep)       # per analisi spaziali
library(sp)          # per analisi spaziali
library(ggplot2)     # per ggplot
library(haven)       # per leggere file STATA
library(tidyverse)   # pacchetto generale
#install.packages("pROC")
library(pROC)
library(dplyr)
library(lmtest)
#install.packages("caret")
library(caret)
library(Metrics)


map <- st_read("#housing data/lucas-county-ohio-census-tracts.shp") # NAD83
map <- st_transform(map, crs=4269) # trasforma in NAD83

data.file <- read_dta(file="#housing data/LucasCountytemp2-NN50.dta") # tipo tibble
data.file <- as.data.frame(data.file)

data.pcg <- house  
head(data.pcg)
data.pcg$id <- 1:dim(data.pcg)[1]

# Small area fixed effects (SAFE)
data.sf <- st_as_sf(data.pcg, coords=c("long", "lat"), crs=2834)
data.sf <- st_transform(data.sf, crs=4269) # trasforma in NAD83
SAFE <- st_join(data.sf, map, join=st_intersects)
data.pcg$SAFE <- as.factor(SAFE$TRACTCE10)

data <- merge(data.pcg, data.file, by.x="id", by.y="id")
data.sf <- st_as_sf(data, coords=c("long", "lat"), crs=2834)  # classe sf
data.sf <- st_transform(data.sf, crs=4269) # converte a NAD83

data.train <- data[data$syear != "1998", ]
data.test <- data[data$syear == "1998", ]
data.train.sf <- data.sf[data.sf$syear != "1998", ]
data.test.sf <- data.sf[data.sf$syear == "1998", ]

#decision tree model
eq <- log_price ~ log_age + log_lotsize + log_livearea + Story2more + wall + beds + baths + dGarage + dToledo + MeanPrice + XWX2M + XWX3M + XWX4M + XWX6M + XWX7M + XWX8M + SAFE
model.tree <- rpart(eq, data = data.train, method = "anova")  # Metodo anova per regressione continua
rpart.plot(model.tree, type=2, extra=101, fallen.leaves=TRUE, cex=0.7)

prediction <- predict(model.tree, data.test)
data.pred.tree <- data.frame(obs=data.test$log_price, pred=prediction)
summary(data.pred.tree)

# Rimozione dei NA dalle previsioni
#missing.tree <- which(is.na(data.pred.tree$pred) == TRUE)
#data.pred.tree <- data.pred.tree[-missing.tree, ]  # elimina i NA

# Visualizzazione dei risultati
#summary(data.pred.tree)


# --------------- TESTs ----------------------
#1) MAE and RMSE
any(is.na(data.pred.tree$obs))
any(is.na(data.pred.tree$pred))
#i found a NA on 2nd f(x)
data.pred.tree <- na.omit(data.pred.tree)
mae_tree <- mae(data.pred.tree$obs, data.pred.tree$pred)
rmse_tree <- rmse(data.pred.tree$obs, data.pred.tree$pred)
cat("MAE:", mae_tree, "\n")
cat("RMSE:", rmse_tree, "\n")
# ------------> sembra peggio rispetto ad OLS stat

# RESIDUALS ANALYSIS-------------
# Compute residuals and store them in a new variable
data.pred.tree$residuals_tree <- data.pred.tree$obs - data.pred.tree$pred

# Save residuals separately as a vector
residuals_tree <- data.pred.tree$residuals_tree

# Convert residuals into a data frame
residuals_df <- data.frame(residuals = residuals_tree)

# Plot residuals analysis ----> histogram 
ggplot(residuals_df, aes(x = residuals)) +
  geom_histogram(aes(y = ..density.., fill = "Histogram"), bins = 30, color = "black", alpha = 0.6) + 
  geom_density(aes(color = "Kernel Density"), size = 1.2) +  
  stat_function(fun = dnorm, args = list(mean = mean(residuals_tree, na.rm = TRUE), 
                                         sd = sd(residuals_tree, na.rm = TRUE)), 
                aes(color = "Normal Distribution"), linetype = "dashed", size = 1) + 
  scale_fill_manual(name = "Legend", values = c("Histogram" = "steelblue")) +  
  scale_color_manual(name = "Legend", values = c("Kernel Density" = "red", "Normal Distribution" = "black")) + 
  labs(title = "Residuals Analysis: Histogram & Density",
       x = "Residuals", y = "Density") +
  theme_classic() +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))

#QQ plot
ggplot(residuals_df, aes(sample = residuals)) +
  stat_qq(aes(color = "Residuals"), size = 1) +  
  stat_qq_line(aes(color = "Theoretical Line"), size = 1, linetype = "solid") +  
  labs(title = "Q-Q Plot of Residuals", 
       x = "Theoretical Quantiles", 
       y = "Sample Quantiles") +
  scale_color_manual(name = "Legend", values = c("Residuals" = "blue", "Theoretical Line" = "red")) +  
  theme_minimal() +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))
# analisi dei residui MOLTO meglio --> qualitá del modello migliore del OLS, da verificare con altri stat test

#Distribution of residuals
ggplot(data = data.pred.tree, aes(x = obs, y = residuals_tree)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Distribution of Residuals", 
       x = "Observed Values", 
       y = "Error (Residual)") +
  theme(legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10))


#4) residuals plot --> c''e etereroskedacity
ggplot(data.frame(Fitted = data.pred.tree$pred, Residuals = data.pred.tree$residuals_tree), 
       aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", 
       x = "Fitted Values", 
       y = "Residuals") +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))

# ------------- Stat tests -------------
# Test: Kolmogorov-Smirnov --> p-value < 0.05: NO Normal distr.
ks.test(residuals_df$residuals, "pnorm", mean(residuals_df$residuals, na.rm = TRUE), sd(residuals_df$residuals, na.rm = TRUE))

# Bresuch Pagan test for residuals
lm_model <- lm(residuals_tree^2 ~ pred, data = data.pred.tree)
bp_test <- bptest(lm_model)
print(bp_test)

#DW test:
dw_test <- dwtest(residuals_tree ~ pred, data = data.pred.tree)
print(dw_test)


# Feature importance plot
importance <- model.tree$variable.importance
importance_df <- data.frame(Feature = names(importance), Importance = importance)

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance", x = "Feature", y = "Importance") +
  theme_minimal()

# Plot the decision tree
rpart.plot(model.tree, type = 2, extra = 101, fallen.leaves = TRUE, cex = 0.1)

#boxplot
ggplot(data.frame(Actual = data.test$log_price, Predicted = data.pred.tree$pred), 
       aes(x = factor(0), y = Actual)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  geom_boxplot(aes(x = factor(1), y = Predicted), fill = "orange", alpha = 0.7) +
  labs(title = "Box Plot of Actual vs Predicted Values", x = "Data", y = "Values") +
  scale_x_discrete(labels = c("Actual", "Predicted")) +  # Adding x-axis labels
  theme_minimal()



# ----------------- ROC and AUC task --------------------
data.train$price_binary <- ifelse(data.train$log_price > median(data.train$log_price, na.rm = TRUE), 1, 0)
data.test$price_binary <- ifelse(data.test$log_price > median(data.test$log_price, na.rm = TRUE), 1, 0)

# eq binary classification
eq_class <- price_binary ~ log_age + log_lotsize + log_livearea + Story2more + wall + beds + baths + 
  dGarage + dToledo + MeanPrice + XWX2M + XWX3M + XWX4M + XWX6M + XWX7M + XWX8M + SAFE
model.tree <- rpart(eq_class, data = data.train, method = "class")

pred_probs <- predict(model.tree, data.test, type = "prob")[, 2]

roc_curve <- roc(data.test$price_binary, pred_probs)

auc_value <- roc_curve$auc
cat("AUC:", auc_value, "\n")

plot(roc_curve, 
     col = "blue",      
     lwd = 2,            
     main = paste("ROC Curve - Decision Tree (AUC =", round(auc_value, 3), ")"), 
     xlab = "False Positive Rate (FPR)",  
     ylab = "True Positive Rate (TPR)",  
     cex.main = 1.5,     
     cex.lab = 1.2,      
     cex.axis = 1.1,     
     font.lab = 2)       
grid()  

pred_class <- ifelse(pred_probs > 0.5, 1, 0)
confusion_matrix <- table(Predicted = pred_class, Actual = data.test$price_binary)
print(confusion_matrix)
conf_matrix <- confusionMatrix(factor(pred_class), factor(data.test$price_binary))
print(conf_matrix)
