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

rownames(sp500) <- as.Date(rownames(sp500))
rownames(slb) <- as.Date(rownames(slb))
rownames(bicsx) <- as.Date(rownames(bicsx))



getSymbols("DCOILWTICO",src="FRED",from=as.Date("2015-09-27"),to=Sys.Date())
oil_df <- as.data.frame(to.monthly(DCOILWTICO[,"DCOILWTICO"],indexAt="lastof",OHLC=FALSE))
rownames(oil_df) <- as.Date(rownames(oil_df))
colnames(oil_df) <- c("oil_ret")

merged_dat <- data.frame(NA)
merged_dat <- merge(sp500,slb)
merged_dat <- merge(merged_dat,bicsx)
merged_dat <- merge(merged_dat,oil_df)
