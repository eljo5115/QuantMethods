# ---------------
# Author: Eli Jordan
# MSBC 5031 - Quant Methods
# Case 4: Are Stock Returns Normal
# ---------------

#installing quantmod
# if quantmod is not already installed
if(!require("quantmod")){
  install.packages("quantmod")
}
library(quantmod)

# change to TRUE to make CSV file of SPY data
writeToCSV <- TRUE

# Get SPY daily data for past 5 years
getSymbols("SPY", src = "yahoo", from = Sys.Date() - 5*365, to = Sys.Date())

head(SPY)

#convert daily to monthly data
SPY_monthly <- to.monthly(SPY,indexAt = "lastof", OHLC = TRUE)
head(SPY_monthly)
# get monthly adjusted data
SPY_monthly_adj <- to.monthly(SPY[,"SPY.Adjusted"], indexAt = "lastof", OHLC = FALSE)
head(SPY_monthly_adj)

# Calculate monthly returns
spy_returns <- diff(SPY_monthly_adj) / lag(SPY_monthly_adj, k = 1)
head(spy_returns)

# Alternatively, using quantmod's Delt function
spy_returns_monthly <- Delt(SPY_monthly_adj, k = 1)
head(spy_returns)

# Convert to a data.frame object
spy_returns <- as.data.frame(spy_returns)

names(spy_returns) <- "spy_ret"

# Get WMT data from Yahoo Finance for the last 5 years
getSymbols("WMT", src = "yahoo", from = Sys.Date() - 5*365, to = Sys.Date())

# Get WMT and SPY data from Yahoo Finance for the last 5 years using just one command... (!)
getSymbols(c("WMT","SPY"), src = "yahoo", from = Sys.Date() - 5*365, to = Sys.Date())

head(WMT)
head(SPY)

#convert daily to monthly data
WMT_monthly <- to.monthly(WMT,indexAt = "lastof", OHLC = TRUE)
head(WMT_monthly)
# get monthly adjusted data
wmt_monthly_adj <- to.monthly(WMT[,"WMT.Adjusted"], indexAt = "lastof", OHLC = FALSE)
head(wmt_monthly_adj)

# Calculate monthly returns
wmt_returns <- diff(wmt_monthly_adj) / lag(wmt_monthly_adj, k = 1)
head(wmt_returns)

# Alternatively, using quantmod's Delt function
wmt_returns_monthly <- Delt(wmt_monthly_adj, k = 1)
head(wmt_returns)

# Convert to a data.frame object
wmt_returns <- as.data.frame(wmt_returns)

names(wmt_returns) <- "wmt_ret"
# Create CSV file with returns
if(writeToCSV){
  # Convert the data to a data frame and write to a CSV file
  write.csv(as.data.frame(spy_returns), file = "SPY_data.csv")
  write.csv(as.data.frame(wmt_returns), file = "WMT_data.csv")
}
# normal mostly, around 0.025
hist(wmt_returns$wmt_ret, main="Histogram of WMT Returns. 8 Breaks",breaks=8)
wmt_mean = mean(wmt_returns$wmt_ret,na.rm=TRUE)
abline(v=wmt_mean,col='red',lty=2)
# normal around 0
hist(spy_returns$spy_ret,main="Histogram of SPY Returns. 8 Breaks",breaks=8)
spy_mean = mean(spy_returns$spy_ret,na.rm=TRUE)
abline(v=spy_mean,col='red',lty=2)
# I could comment on how these graphs have relatively skinny tails (1 occurence of >2sd) but 
# the sample size isn't large enough to draw an absolute conclusion especially since 
# stock returns ~should~ have pretty fat tails (leptokurtic) around +- 3sd. However this data does not support
# that conclusion so I must remain silent. 

# new graphs, 20 breaks
# 50 breaks was almost too much granularity, hard to see dist shape
hist(wmt_returns$wmt_ret,breaks=20, main = "WMT Returns 20 breaks")
hist(spy_returns$spy_ret,breaks=20,main = "WMT Returns 20 breaks")

# create logical vector of no jumps
find_jumps_wmt = abs(wmt_returns$wmt_ret) < 0.03
# create new vector of no jumps
no_jumps_wmt = wmt_returns$wmt_ret[find_jumps_wmt]
# plot
hist(no_jumps_wmt,breaks=10)

# create logical vector of no jumps
find_jumps_spy = abs(spy_returns$spy_ret) < 0.03
# create new vector of no jumps
no_jumps_spy = spy_returns$spy_ret[find_jumps_spy]
# plot
hist(no_jumps_spy,breaks=10)

#5th percentile returns for walmart
quantile(wmt_returns$wmt_ret,probs=0.05,na.rm=TRUE) # - 0.06 = -6%
#5th percentile returns for spy
quantile(spy_returns$spy_ret,probs=0.05,na.rm=TRUE) # -0.058 = -5.8%

# These answers make sense as the lowest 5% of returns are around -20% - 0% so this quantile should be negative.
quantile(spy_returns_monthly, probs=0.05, na.rm=TRUE)
quantile(wmt_returns_monthly, probs=0.05, na.rm=TRUE)
# Same answers for monthly returns, this makes sense as the monthly returns are aggregate across days.

set.seed(42)

# First, calculate the standard deviation for WMT returns
wmt_sd <- sd(wmt_returns$wmt_ret, na.rm = TRUE)

# Plot the histogram with the y-axis as density
hist(wmt_returns$wmt_ret,
     breaks = 10,
     probability = TRUE,
     main = "Histogram of WMT Returns with Normal Curve",
     xlab = "Monthly Returns",
     ylab = "Density",
     col = "lightblue")

# Use the curve() function for a simpler way to add the normal distribution line
curve(dnorm(x, mean = wmt_mean, sd = wmt_sd),
      col = "red",
      lwd = 2,
      add = TRUE)

# Add a legend to make the plot easier to understand
legend("topright", legend = "Normal Curve", col = "red", lwd = 2, bty = "n")

# From the plot, we can see that the normal distribution for $WMT approximates the actual
# returns pretty well.

spy_sd <- sd(spy_returns$spy_ret, na.rm = TRUE)

# Plot the histogram with the y-axis as density
hist(spy_returns$spy_ret,
     breaks = 10,
     probability = TRUE,
     main = "Histogram of SPY Returns with Normal Curve",
     xlab = "Monthly Returns",
     ylab = "Density",
     col = "lightblue")

# Use the curve() function for a simpler way to add the normal distribution line
curve(dnorm(x, mean = spy_mean, sd = spy_sd),
      col = "red",
      lwd = 2,
      add = TRUE)

# Add a legend to make the plot easier to understand
legend("topright", legend = "Normal Curve", col = "blue", lwd = 2, bty = "n")

# However for SPY, the normal distribution appears to miss the tails of 
# the actual returns. 