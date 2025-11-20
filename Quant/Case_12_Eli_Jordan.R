# ----------
# Author: Eli Jordan
# Case 12, Multiple Regression Ames Data
# ----------

setwd("/Users/eli/noticloud/Documents/GitHub/QuantMethods/Quant/data")
load("ames.Rdata")

ames_trim <- ames[,c("SalePrice","Lot.Area","Gr.Liv.Area","Yr.Sold","Year.Built")]
ames_trim$Age <- ames_trim$Yr.Sold - ames_trim$Year.Built

# ------------------------ Histograms ------------------------

# sale price histogram
mean(ames_trim$SalePrice) # 180796.1
sd(ames_trim$SalePrice) # 79886.69
hist(ames_trim$SalePrice, main = "Sale Price", xlab = "Price", ylab = "Frequency")
# Skewed left, slopes steep downwards after ~3e05

# lot area histogram
mean(ames_trim$Lot.Area) # 10147.92
sd(ames_trim$Lot.Area) # 7880.018
hist(ames_trim$Lot.Area, main = "Lot Area", xlab = "Area", ylab = "Frequency",breaks=50,xlim=c(0,75000))
# clustered around 10,000

# above ground living area histogram
mean(ames_trim$Gr.Liv.Area) # 1499.69
sd(ames_trim$Gr.Liv.Area) # 505.5089
hist(ames_trim$Gr.Liv.Area, main = "Above Ground Living Area", xlab = "Area", ylab = "Frequency",breaks=20)
# clustered around 1500, few outliers below 1000

# Year sold histogram
mean(ames_trim$Yr.Sold) #2007.79
sd(ames_trim$Yr.Sold) # 1.316613
hist(ames_trim$Yr.Sold, main = "Year Sold", xlab = "Year", ylab = "Frequency",breaks=20)
# mostly uniform by year 2006-2010

# year built histogram
mean(ames_trim$Year.Built) # 1971.356
sd(ames_trim$Year.Built) # 30.24536
hist(ames_trim$Year.Built, main = "Year Built", xlab = "Year", ylab = "Frequency",breaks=20)
# increasing to 2010

# Age histogram
mean(ames_trim$Age) # 36.43413
sd(ames_trim$Age) # 30.29136
hist(ames_trim$Age, main = "Age", xlab = "Age", ylab = "Frequency",breaks=20)
# inverse of year built, makes sense from year built and year sold distributions

# ------------------------ LM Regressions ------------------------
lot_lm <- lm(SalePrice~Lot.Area, data = ames_trim)
summary(lot_lm)
# R-squared 0.07073
# Intercept: 153400
# Slope: 2.702
# large t-values; statistically significant.

# filter outliers and regress
lot_lm_filtered <- lm(SalePrice ~ Lot.Area, data = ames_trim[ames_trim$Lot.Area < 100000 & ames_trim$SalePrice < 550000, ])
summary(lot_lm_filtered)
# R-squared: 0.1603 (Improved from 0.071)
# intercept: 1.338e+05
# slope: 4.552
# larger t-values; statistically significant

liv_area_lm <- lm(SalePrice~Gr.Liv.Area, data = ames_trim)
summary(liv_area_lm)
# R-squared: 0.4994
# Intercept: 13289.634
# Slope: 111.694
# t-values large; statistically significant

liv_area_lm_filtered <- lm(SalePrice ~ Gr.Liv.Area, data = ames_trim[ames_trim$Gr.Liv.Area < 4000 & ames_trim$SalePrice < 550000,])
summary(liv_area_lm_filtered)
# R-squared: 0.548 (Improved from 0.499)

age_lm <- lm(SalePrice~Age, data = ames_trim)
summary(age_lm)
# R-squared: 0.3121
# Intercept: 234499.68
# Slope: -1473.99
# large t-values; statistically significant

age_lm_filtered <- lm(SalePrice ~ Age, data = ames_trim[ames_trim$Age < 110 & ames_trim$SalePrice < 550000,])
summary(age_lm_filtered)
# R-squared: 0.352 (Improved from 0.312)

# ------------------------ Plots ------------------------

plot(SalePrice ~ Lot.Area, data = ames_trim, main = "Sale Price vs Lot Area"
  ,xlab = "Lot Area", ylab = "Sale Price",pch = "+",cex=0.5,las=1)
abline(lot_lm,col="red",lty=2)
# plot lot area mean
abline(v=mean(ames_trim$Lot.Area,na.rm=TRUE),col="blue",lty=2)
# plot sale price mean
abline(h=mean(ames_trim$SalePrice,na.rm=TRUE),col="blue",lty=2)
# color outlier points
points(SalePrice ~ Lot.Area, data=ames_trim[ames_trim$Lot.Area>100000, ],pch="+",col="red",cex=1,las=1)
points(SalePrice ~ Lot.Area, data=ames_trim[ames_trim$SalePrice>550000, ],pch="+",col="red",cex=1,las=1)
cor(ames_trim$Lot.Area,ames_trim$SalePrice) # 0.2665492
# Theres a few outliers dragging the slope down from being more vertical, there is a huge cluster of points
# between 0-50000
abline(lot_lm_filtered, lty=2, col="green")
# R-squared: 0.1603 (Improved from 0.071
# the outlier-removed regression has a much higher slope and a lower intercept, this makes
# sense since the biggest outliers were huge lot areas and low sale prices so they were
# pulling the slope downward

plot(SalePrice~Gr.Liv.Area, data = ames_trim, main = "Sale Price vs Above Ground Living Area"
  ,xlab = "Above Ground Living Area", ylab = "Sale Price",pch="+",cex=0.5,las=1)
abline(liv_area_lm,col="red",lty=2)
abline(v=mean(ames_trim$Gr.Liv.Area,na.rm=TRUE),col="blue",lty=2)
abline(h=mean(ames_trim$SalePrice,na.rm=TRUE),col="blue",lty=2)
cor(ames_trim$Gr.Liv.Area,ames_trim$SalePrice) # 0.7067799
# This correlation number is what my guess would be, there is a clear strong positive correlation
# with few outliers, however the outliers are definitely dragging it down a bit
points(SalePrice ~ Gr.Liv.Area, data=ames_trim[ames_trim$Gr.Liv.Area>4000, ],pch="+",col="red",cex=1,las=1)
points(SalePrice ~ Gr.Liv.Area, data=ames_trim[ames_trim$SalePrice>550000, ],pch="+",col="red",cex=1,las=1)
abline(liv_area_lm_filtered, lty=2, col="green")
# you probably only see a green line, that would be because there is no change after removing the outliers

plot(SalePrice~Age, data = ames_trim, main = "Sale Price vs Age", xlab = "Age"
  ,ylab = "Sale Price",pch="+"
  ,cex=0.5,las=1)
abline(age_lm,col="red",lty=2)
abline(v=mean(ames_trim$Age,na.rm=TRUE),col="blue",lty=2)
abline(h=mean(ames_trim$SalePrice,na.rm=TRUE),col="blue",lty=2)
cor(ames_trim$Age,ames_trim$SalePrice) # -0.5589068
# This is a clear negative correlation across all of the sale prices. This makes sense as older homes tend
# to be less expensive than new builds. There are a few outliers, however the main body is clustered tightly
# and numerously enough they probably have a small effect
points(SalePrice ~ Age, data=ames_trim[ames_trim$Age>110, ],pch="+",col="red",cex=1,las=1)
points(SalePrice ~ Age, data=ames_trim[ames_trim$SalePrice>550000, ],pch="+",col="red",cex=1,las=1)
abline(age_lm_filtered, lty=2, col="green")
# the most (only) significant change in the regression w/o outliers is the intercept

# ---------------------- Log Plots ------------------------
# log resulted in some NaN for Ages=0
# so I use asinh; close enough to log
asinh_ames_trim <- asinh(ames_trim)

lm_lot_asinh <- lm(SalePrice~Lot.Area, data = asinh_ames_trim)
summary(lm_lot_asinh)
# intercept: 9.8388
# slope: 0.2938
# R-squared: 0.1355

lm_liv_asinh <- lm(SalePrice~Gr.Liv.Area, data = asinh_ames_trim)
summary(lm_liv_asinh)
# intercept: 5.494
# slope: 0.9078
# R-squared: 0.5228

lm_age_asinh <- lm(SalePrice~Age, data = asinh_ames_trim)
summary(lm_age_asinh)
# intercept: 13.336
# slope: -0.1718
# R-squared: 0.3911

plot(SalePrice~Lot.Area, data = asinh_ames_trim, main = "Asinh Sale Price vs Asinh Lot Area"
  ,xlab = "Asinh Lot Area", ylab = "Asinh Sale Price",pch="+",cex=0.5,las=1)
abline(lm_lot_asinh,col="red",lty=2)


plot(SalePrice~Gr.Liv.Area, data = asinh_ames_trim, main = "Asinh Sale Price vs Asinh Above Ground Living Area"
  ,xlab = "Asinh Above Ground Living Area", ylab = "Asinh Sale Price",pch="+",cex=0.5,las=1)
abline(lm_liv_asinh,col="red",lty=2)


plot(SalePrice~Age, data = asinh_ames_trim, main = "Asinh Sale Price vs Asinh Age", xlab = "Asinh Age"
  ,ylab = "Asinh Sale Price",pch="+"
  ,cex=0.5,las=1)
abline(lm_age_asinh,col="red",lty=2)


# The slopes of the log graphs could be considered the elasticity of the sale price with the
# above ground living area, lot area, and age.

# The slope coefficients would be considered percentage change of X for percentage change of Y,
# This is how we get elasticity as previously mentioned, but the percent change for percent change
# can be useful on it's own.
# Also the intercept is the percentage change of Y when X changes by 0.