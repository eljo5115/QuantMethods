# -----------
# Author: Eli Jordan
# Case 13: Multiple Regression
# -----------


if(!require("quantmod")){
  install.packages("quantmod")
}
library(quantmod)

tickersToAnalyze <- c("^GSPC","SLB","BICSX")
getSymbols(tickersToAnalyze,src="yahoo",from=as.Date("2025-10-31")-61*31,to=as.Date("2025-10-31"),periodicity="monthly")
sp500 <- as.data.frame(Delt(to.monthly(GSPC[,"GSPC.Adjusted"],indexAt="lastof",OHLC=FALSE),k=1))
slb <- as.data.frame(Delt(to.monthly(SLB[,"SLB.Adjusted"],indexAt="lastof",OHLC=FALSE),k=1))
bicsx <- as.data.frame(Delt(to.monthly(BICSX[,"BICSX.Adjusted"],indexAt="lastof",OHLC=FALSE),k=1))
colnames(sp500) <- c("sp500_ret")
colnames(slb) <- c("slb_ret")
colnames(bicsx) <- c("bicsx_ret")

sp500$Date <- as.Date(row.names(sp500))
slb$Date <- as.Date(row.names(slb))
bicsx$Date <- as.Date(rownames(bicsx))



getSymbols("DCOILWTICO",src="FRED",from=as.Date("2015-09-27"),to=Sys.Date())
oil_df <- as.data.frame(Delt(to.monthly(DCOILWTICO[,"DCOILWTICO"],indexAt="lastof",OHLC=FALSE),k=1))
oil_df$Date <- as.Date(row.names(oil_df))
names(oil_df) <- c("oil_ret","Date")


merged_dat <- data.frame(NA)
merged_dat <- merge(sp500,slb,all.x = FALSE)
merged_dat <- merge(merged_dat,bicsx, all.x=FALSE)
merged_dat <- merge(merged_dat,oil_df, by="Date",all.x=FALSE)


summary(merged_dat)

sp_mean <- mean(merged_dat$sp500_ret,na.rm=TRUE) # 0.012716
slb_mean <- mean(merged_dat$slb_ret,na.rm= TRUE) # 0.02238155
bicsx_mean <- mean(merged_dat$bicsx_ret,na.rm= TRUE) # 0.01112157
oil_mean <- mean(merged_dat$oil_ret,na.rm= TRUE)# 0.009842804

sp_sd <- sd(merged_dat$sp500_ret,na.rm=TRUE) #0.04527616
slb_sd <- sd(merged_dat$slb_ret,na.rm=TRUE) # 0.1218169
bicsx_sd <- sd(merged_dat$bicsx_ret,na.rm=TRUE) # 0.04273958
oil_sd <- sd(merged_dat$oil_ret,na.rm=TRUE) # 0.08886552

plot(slb_ret ~ sp500_ret, data=merged_dat, xlab="S&P 500", ylab="SLB", pch="+", cex=0.5)
slb_sp_lm <- lm(slb_ret~sp500_ret,data=merged_dat)
summary(slb_sp_lm)
#Intercept: 0.00975, large p 
# Slope: 0.9756, small p **
abline(slb_sp_lm, col="red",lty=2)

plot(slb_ret ~ oil_ret, data=merged_dat, xlab="Oil Returns", ylab="SLB", pch="+", cex=0.5)
slb_oil_lm <- lm(slb_ret~oil_ret,data=merged_dat)
summary(slb_oil_lm)
# Intercept: 0.01260,  large p
# Slope: 0.89, small p ***
abline(slb_oil_lm, col="red",lty=2)

plot(bicsx_ret ~ sp500_ret, data=merged_dat, xlab="S&P 500", ylab="BICSX", pch="+", cex=0.5)
bicsx_sp_lm <- lm(bicsx_ret~sp500_ret,data=merged_dat)
summary(bicsx_sp_lm)
# Intercept: 0.00681, large p 
# Slope: 0.3389, small p **
abline(lm(bicsx_ret~sp500_ret,data=merged_dat), col="red",lty=2)

plot(bicsx_ret ~oil_ret, data=merged_dat,xlab="Oil Returns", ylab="BICSX", pch="+", cex=0.5)
bicsx_oil_lm <- lm(bicsx_ret~oil_ret,data=merged_dat)
summary(bicsx_oil_lm)
# Intercept: 0.0089 small p
# Slope: 0.2027, small p ***
abline(bicsx_oil_lm, col="red",lty=2)

# from the lines and scatterplots, it seems SLB and BISCX have positive correlations with
# both the SP500 and Crude Oil Returns.

slb_lm <- lm(slb_ret~sp500_ret+oil_ret,data=merged_dat)
summary(slb_lm)
# Estimate single regression:
# SP500: 0.89
# Oil: 0.3389
# Estimate multiple regression:
# SP500: 0.673 *
# Oil: 0.826 ***
# From the multple regression we can see that SLB has a much higher correlation to the
# price of oil than the SP500. This can explain the discrepancies between the multiple
# and single regression.

# Holding constant the SP500, SLB would average 0.826 returns for every point that oil
# moves on average.

bicsx_lm <- lm(bicsx_ret~sp500_ret+oil_ret,data=merged_dat)
summary(bicsx_lm)
# Estimate single regression:
# SP500: 0.3389
# Oil: 0.2027
# Estimate multiple regression:
# SP500: 0.274 *
# Oil: 0.1768 **
# The multiple regression moved much less than SLB, this would make sense for an ETF
# with holdings across the commodities sector and much less focused in only oil like SLB
# We should expect some correlation with Oil from their oil company holdings, but this would
# be offset from other holdings.

# Holding constant the SP500, BISCX would average 0.1768 returns for every point that oil
# moves on average.
setwd("/Users/eli/noticloud/Documents/GitHub/QuantMethods/Quant/data")
source("binscatter_function.R")

binscatter("slb_ret~sp500_ret+oil_ret", key_var = "oil_ret", 
           partial=TRUE, data=merged_dat, bins = 10)

binscatter("bicsx_ret~sp500_ret+oil_ret", key_var = "oil_ret", 
  partial=TRUE, data=merged_dat, bins = 10)
           
# The single regression intercept is the returns of SLB when oil returns are 0.
# However, the multiple regression intercept is the returns of SLB when oil returns are 0
# and the SP500 returns are 0. Same for BICSX.

# SLB's multiple regression coefficient with SP500 is 0.673. This is an average across
# the points plotted on the binscatter graph. This is the slope of the blue line. 
# What this really means is the correlation dependent on the oil returns.

# Similar for BICSX, the multiple regression coefficient with SP500 is 0.274.

# In the multiple regression the market risk (Beta) is the SP500 coefficient
# so for SLB: 0.673 and BICSX: 0.274. This controls for oil.