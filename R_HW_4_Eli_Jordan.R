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
writeToCSV <- FALSE

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

if(writeToCSV){
  # Convert the data to a data frame and write to a CSV file
  write.csv(as.data.frame(spy_returns), file = "SPY_data.csv")
}

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
head(WMT_monthly_adj)

# Calculate monthly returns
wmt_returns <- diff(WMT_monthly_adj) / lag(WMT_monthly_adj, k = 1)
head(wmt_returns)

# Alternatively, using quantmod's Delt function
wmt_returns_monthly <- Delt(wmt_monthly_adj, k = 1)
head(wmt_returns)

# Convert to a data.frame object
wmt_returns <- as.data.frame(wmt_returns)

names(wmt_returns) <- "wmt_ret"

# normal mostly, around 0.025
hist(wmt_returns$wmt_ret, main="Histogram of WMT Returns. 8 Breaks",breaks=8)
# Incredibly normal around 0
hist(spy_returns$spy_ret,main="Histogram of SPY Returns. 8 Breaks",breaks=8)
# I could comment on how these graphs have relatively skinny tails (1 occurence of >2sd) but 
# the sample size isn't large enough to draw an absolute conclusion especially since 
# stock returns ~should~ have pretty fat tails (leptokurtic) around +- 3sd. However this data does not support
# that conclusion so I must remain silent. 

# new graphs, 20 breaks
# 50 breaks was almost too much granularity, hard to see dist shape
hist(wmt_returns$wmt_ret,breaks=20, main = "WMT Returns 20 breaks")
hist(spy_returns$spy_ret,breaks=20,main = "WMT Returns 20 breaks")
