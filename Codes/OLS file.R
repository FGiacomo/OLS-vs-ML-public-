## --------- FINAL DEPLOYMENT VERSION -------------
# Giacomo Fantato ---- K-16958 - g.fantato@student.uw.edu.pl

## ----------- OLS regression Analysis -----------------------
setwd("C:\\UW\\Second Semester\\Social Networks in Economics Geo\\Project - OLS vs ML")
wd = getwd()
#install.packages(c("sf", "spdep", "sp", "varycoef", "ggplot2", "haven", "tidyverse"))
library(sf)		# for spatial analysis
library(spdep)		# for spatial analysis
library(sp)		# for spatial analysis
library(varycoef)	# original dataset house
library(ggplot2)		# for (spatial) ggplot
library(haven)		# to read STATA file
library(tidyverse)
# optional packages: to assess the quality of predictions
#install.packages(c("metrica", "dplyr", "purrr", "tidyr"))
library(metrica) # https://adriancorrendo.github.io/metrica/ 
library(dplyr)
library(purrr)
library(tidyr)
#install.packages("oce")
library(oce) 
#install.packages("Metrics")
library(Metrics)
library(lmtest)
#install.packages("nortest") 
library(nortest)
#NB: vareycoeff is deprecated!! --> use install it locally:
#install.packages("C:\\UW\\Second Semester\\Social Networks in Economics Geo\\Project - OLS vs ML\\varycoef_0.3.4.tar.gz", repos = NULL, type="source")

# reading Lucas country census track
# https://koordinates.com/layer/99834-lucas-county-ohio-census-tracts/ 
map<-st_read("#housing data/lucas-county-ohio-census-tracts.shp") # NAD83
map<-st_transform(map, crs=4269) # to NAD83

# reading data from STATA
data.file<-read_dta(file="#housing data/LucasCountytemp2-NN50.dta") # class tibble
data.file<-as.data.frame(data.file)

# reading original ‘house’ data from package {varycoef}
data.pcg<-house		# data from package
head(data.pcg)
data.pcg$id<-1:dim(data.pcg)[1]

# small area fixed effects - SAFE
# we get the ID of the region for each observation
data.sf<-st_as_sf(data.pcg, coords=c("long", "lat"), crs=2834)
data.sf<-st_transform(data.sf, crs=4269) # to NAD83
SAFE<-st_join(data.sf, map, join=st_intersects)
data.pcg$SAFE<-as.factor(SAFE$TRACTCE10)

# merge of datasets – raw and transformed
data<-merge(data.pcg, data.file, by.x="id", by.y="id")
data.sf<-st_as_sf(data, coords=c("long", "lat"), crs=2834)# sf class
data.sf<-st_transform(data.sf, crs=4269) # convert to NAD83

# split into train (up to year 1997) and test (year 1998) data
data.train<-data[data$syear!="1998",]
data.test<-data[data$syear=="1998",]
data.train.sf<-data.sf[data.sf$syear!="1998",]
data.test.sf<-data.sf[data.sf$syear=="1998",]

# plot of data and map: all dataset plot
plot(st_geometry(map), mar=c(1,1,1,1))
plot(st_geometry(data.sf), add=TRUE, bg="red", pch=21)
degAxis(1)
degAxis(2)

# ---------- Plot for TRAIN SET ---------------
plot(st_geometry(map), mar=c(1,1,1,1))
plot(st_geometry(data.train.sf), add=TRUE, bg="red", pch=21)
title("Train set - OLS regression")
degAxis(1)
degAxis(2)

# ------------------- plot for DATA SET --------------
plot(st_geometry(map), mar=c(1,1,1,1))
plot(st_geometry(data.test.sf), add=TRUE, bg="red", pch=21)
title("Data set - OLS regression")
degAxis(1)
degAxis(2)

# Starter of the model
eq<-log_price ~ log_age + log_lotsize + log_livearea + Story2more + wall + beds + baths + dGarage + dToledo + MeanPrice + XWX2M + XWX3M + XWX4M + XWX6M + XWX7M + XWX8M + SAFE

# OLS model
model.lm<-lm(eq, data.train)
summary(model.lm)

# mapping coefficients of SAFE (for small districts)
map$SAFE.ID<-paste0(rep("SAFE", times=dim(map)[1]), map$TRACTCE10)
coefs<-as.data.frame(model.lm$coefficients)
vec<-rownames(coefs)
coefs2<-data.frame(name=vec, coefs)
colnames(coefs2)<-c("name", "value")
map<-merge(map, coefs2, by.x="SAFE.ID", by.y="name", sort=FALSE, all.x=TRUE)

ggplot(data=map) + geom_sf(aes(fill=value))+   scale_fill_viridis_c(option="plasma") + labs(title="SAFE – Small Area Fixed Effects - OLS")

# running predictions
prediction<-predict.lm(model.lm, data.test)
data.pred.OLS<-data.frame(obs=data.test$log_price, pred=prediction)
missing.OLS<-which(is.na(data.pred.OLS$pred)==TRUE)
data.pred.OLS<-data.pred.OLS[-missing.OLS,] # to eliminate NAs

#------------------------------TESTs-----------------------------------------------

summary(data.pred.OLS)

#MAE:
# Calcolo dell'errore medio assoluto (MAE) e dell'errore quadratico medio (RMSE)
#Preliminar operations: MAE not works with NA values --> check if NA is present there:
any(is.na(data.pred.OLS$obs))
any(is.na(data.pred.OLS$pred))
#i found a NA on 2nd f(x)
data.pred.OLS <- na.omit(data.pred.OLS)
mae_OLS <- mae(data.pred.OLS$obs, data.pred.OLS$pred)
rmse_OLS <- rmse(data.pred.OLS$obs, data.pred.OLS$pred)
cat("MAE:", mae_OLS, "\n")
cat("RMSE:", rmse_OLS, "\n")

#-------------------------- Residuals Analysis plot -----------------------

#1) 
# Istogramma con densità --> sembrano normali i residui a causa della gaussiana creata 
# Istogramma con densità e legenda
ggplot(data.frame(residuals_OLS), aes(x=residuals_OLS)) +
  geom_histogram(aes(y=..density.., fill="Histogram"), bins=30, color="black", alpha=0.7) + 
  geom_density(aes(color="Density"), size=1) +  
  scale_fill_manual(name = "Legend", values = c("Histogram" = "steelblue")) +  
  scale_color_manual(name = "Legend", values = c("Density" = "red")) + 
  labs(title="Histogram of Residuals", x="Residuals", y="Density") +
  theme_minimal() +
  theme(legend.title = element_text(size=12), legend.text = element_text(size=10))  

# 2) Q-Q plot con legenda
ggplot(data.frame(residuals_OLS), aes(sample = residuals_OLS)) +
  stat_qq(aes(color = "Residuals"), size=1) +  
  stat_qq_line(aes(color = "Theoretical Line"), size=1, linetype="solid") +  
  labs(title = "Q-Q Plot of Residuals", 
       x = "Theoretical Quantiles", 
       y = "Sample Quantiles") +
  scale_color_manual(name = "Legend", values = c("Residuals" = "blue", "Theoretical Line" = "red")) +  
  theme_minimal() +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))  



# 3) observed vs residuals
ggplot(data = data.pred.OLS, aes(x = obs, y = obs - pred)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Distribution of Residuals", x = "Observed Values", y = "Error (Residual)") +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))  

#4) residuals plot --> c''e etereroskedacity
ggplot(data.frame(Fitted=model.lm$fitted.values, Residuals=residuals_OLS), 
       aes(x=Fitted, y=Residuals)) +
  geom_point(alpha=0.5, color="blue") +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  labs(title="Residuals vs Fitted Values", x="Fitted Values", y="Residuals") +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))  
#
residuals_OLS <- residuals(model.lm)

#Saphiro Test --> not robust, not working propery because the #obs is > 5000 (better use Kolmogorov-Smirnov)
shapiro.test(data.pred.OLS$obs - data.pred.OLS$pred)

#shapiro.test(residuals_OLS)  # Test valido per piccoli campioni (<5000)
# KS - Test 

# Test: Kolmogorov-Smirnov --> p-value < 0.05: NO Normal distr.
ks.test(residuals_OLS, "pnorm", mean(residuals_OLS), sd(residuals_OLS))

#Test di Anderson-Darling: p-value < 0.05: NO Normal distr.
ad.test(residuals_OLS)

# statistical tests
dwtest(model.lm)  # Test di Durbin-Watson --> Un valore di Durbin-Watson vicino a 2 indica che non c’è autocorrelazione.

bptest(model.lm)  # Test di Breusch-Pagan per l'eteroschedasticità
# Se il p-value è < 0.05, c’è eteroschedasticità (varianza non costante → possibile problema nel modello).

# -------------- feature selection
step_model <- step(lm(eq, data = data.train), direction = "both", trace = TRUE)
summary(step_model)

#No spatial vars:
eq_no_spatial <- log_price ~ log_age + log_lotsize + log_livearea + Story2more + wall + beds + baths + dGarage
model_no_spatial <- lm(eq_no_spatial, data.train)
summary(model_no_spatial)
prediction_spatial <- predict.lm(model.lm, data.test)
data.pred_spatial <- data.frame(obs=data.test$log_price, pred=prediction_spatial)
data.pred_spatial <- na.omit(data.pred_spatial)
mae_spatial <- mae(data.pred_spatial$obs, data.pred_spatial$pred)
rmse_spatial <- rmse(data.pred_spatial$obs, data.pred_spatial$pred)
#spatial vars
prediction_no_spatial <- predict.lm(model_no_spatial, data.test)
data.pred_no_spatial <- data.frame(obs=data.test$log_price, pred=prediction_no_spatial)
data.pred_no_spatial <- na.omit(data.pred_no_spatial)
# No spatial vars:
mae_no_spatial <- mae(data.pred_no_spatial$obs, data.pred_no_spatial$pred)
rmse_no_spatial <- rmse(data.pred_no_spatial$obs, data.pred_no_spatial$pred)
cat("MAE (NO spatial):", mae_no_spatial, "\n")
cat("RMSE (NO spatial):", rmse_no_spatial, "\n")
cat("MAE (Spatial model):", mae_spatial, "\n")
cat("RMSE (Spatial model):", rmse_spatial, "\n")


#graph:
# Residuals spatial vs non-spatial model
ggplot() +
  geom_point(data=data.pred_spatial, aes(x=obs, y=obs - pred, color="Spatial Model"), alpha=0.5) +
  geom_point(data=data.pred_no_spatial, aes(x=obs, y=obs - pred, color="Non-Spatial Model"), alpha=0.5) +
  labs(title="Distribution of Errors: Spatial Model vs Non-Spatial Model", 
       x="Observed Values", 
       y="Residuals") +
  scale_color_manual(name = "Model", 
                     values = c("Spatial Model" = "blue", "Non-Spatial Model" = "red")) +
  theme_minimal()

