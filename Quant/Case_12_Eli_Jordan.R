# ----------
# Author: Eli Jordan
# Case 12, Multiple Regression Ames Data
# ----------

setwd("/Users/eli/noticloud/Documents/GitHub/QuantMethods/Quant/data")
load("ames.Rdata")

ames_trim <- ames[,c("SalePrice","Lot.Area","Gr.Liv.Area","Yr.Sold","Year.Built")]
ames_trim$Age <- ames_trim$Yr.Sold - ames_trim$Year.Built

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

plot(SalePrice ~ Lot.Area, data = ames_trim, main = "Sale Price vs Lot Area"
  ,xlab = "Lot Area", ylab = "Sale Price",pch = "+",cex=0.5,las=1)
abline(a=153400,b=2.702,col="red",lty=2)
abline(v=mean(ames_trim$Lot.Area,na.rm=TRUE),col="blue",lty=2)
abline(h=mean(ames_trim$SalePrice,na.rm=TRUE),col="blue",lty=2)
cor(ames_trim$Lot.Area,ames_trim$SalePrice) # 0.2665492
# Theres a few outliers dragging the slope down from being more vertical, there is a huge cluster of points
# between 0-50000
plot(SalePrice~Gr.Liv.Area, data = ames_trim, main = "Sale Price vs Above Ground Living Area"
  ,xlab = "Above Ground Living Area", ylab = "Sale Price",pch="+",cex=0.5,las=1)
abline(a=13289.634,b=111.694,col="red",lty=2)
abline(v=mean(ames_trim$Gr.Liv.Area,na.rm=TRUE),col="blue",lty=2)
abline(h=mean(ames_trim$SalePrice,na.rm=TRUE),col="blue",lty=2)
cor(ames_trim$Gr.Liv.Area,ames_trim$SalePrice) # 0.7067799
# This correlation number is what my guess would be, there is a clear strong positive correlation
# with few outliers, however the outliers are definitely dragging it down a bit
plot(SalePrice~Age, data = ames_trim, main = "Sale Price vs Age", xlab = "Age"
  ,ylab = "Sale Price",pch="+"
  ,cex=0.5,las=1)
abline(a=234499.68,b=-1437.99,col="red",lty=2)
abline(v=mean(ames_trim$Age,na.rm=TRUE),col="blue",lty=2)
abline(h=mean(ames_trim$SalePrice,na.rm=TRUE),col="blue",lty=2)
cor(ames_trim$Age,ames_trim$SalePrice) # -0.5589068
# This is a clear negative correlation across all of the sale prices. This makes sense as older homes tend
# to be less expensive than new builds. There are a few outliers, however the main body is clustered tightly
# and numerously enough they probably have a small effect

lot_lm <- lm(SalePrice~Lot.Area, data = ames_trim)
summary(lot_lm)
# R-squared 0.07073
# Intercept: 153400
# Slope: 2.702
# large t-values; statistically significant
liv_area_lm <- lm(SalePrice~Gr.Liv.Area, data = ames_trim)
summary(liv_area_lm)
# R-squared: 0.4994
# Intercept: 13289.634
# Slope: 111.694
# t-values large; statistically significant
age_lm <- lm(SalePrice~Age, data = ames_trim)
summary(age_lm)
# R-squared: 0.3121
# Intercept: 234499.68
# Slope: -1473.99
# large t-values; statistically significant
