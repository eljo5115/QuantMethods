# ---------------
# Author: Eli Jordan
# MSBC 5031 - Quant Methods
# Case 8: Midterm (P)review
# ---------------

#installing quantmod
# if quantmod is not already installed
if(!require("quantmod")){
  install.packages("quantmod")
}
library(quantmod)

# ------------------------------- Question 1: Returns --------------------------
getSymbols(c("^DJI","^GSPC","MRNA","NFLX"), src = "yahoo", from = "2020-01-01", to = Sys.Date())

djia <- as.data.frame(DJI)
sp500 <- as.data.frame(GSPC)
mrna <- as.data.frame(MRNA)
nflx <- as.data.frame(NFLX)

# Convert to monthly
dji_monthly <- to.monthly(DJI,indexAt = "lastof", OHLC = TRUE)
sp500_monthly <- to.monthly(GSPC,indexAt = "lastof", OHLC = TRUE)
mrna_monthly <- to.monthly(MRNA,indexAt = "lastof", OHLC = TRUE)
nflx_monthly <- to.monthly(NFLX,indexAt = "lastof", OHLC = TRUE)
head(dji_monthly)
# get monthly adjusted data
dji_monthly_adjusted <- to.monthly(DJI[,"DJI.Adjusted"], indexAt = "lastof", OHLC = FALSE)
sp500_monthly_adjusted <- to.monthly(GSPC[,"GSPC.Adjusted"], indexAt = "lastof", OHLC = FALSE)
mrna_monthly_adjusted <- to.monthly(MRNA[,"MRNA.Adjusted"], indexAt = "lastof", OHLC = FALSE)
nflx_monthly_adjusted <- to.monthly(NFLX[,"NFLX.Adjusted"], indexAt = "lastof", OHLC = FALSE)
head(dji_monthly_adjusted)
# Calculate monthly returns
dji_monthly_returns <- Delt(dji_monthly_adjusted, k = 1)
sp500_monthly_returns <- Delt(sp500_monthly_adjusted, k = 1)
mrna_monthly_returns <- Delt(mrna_monthly_adjusted, k = 1)
nflx_monthly_returns <- Delt(nflx_monthly_adjusted, k = 1)
head(dji_monthly_returns)
# Calculate daily returns
dji_daily_returns <- Delt(DJI$DJI.Adjusted, k = 1)
sp500_daily_returns <- Delt(GSPC$GSPC.Adjusted, k = 1)
mrna_daily_returns <- Delt(MRNA$MRNA.Adjusted, k = 1)
nflx_daily_returns <- Delt(NFLX$NFLX.Adjusted, k = 1)
head(dji_daily_returns)

# create gross returns column
djia$gross_ret <- djia$DJI.Adjusted/djia$DJI.Adjusted[1]
sp500$gross_ret <- sp500$GSPC.Adjusted/sp500$GSPC.Adjusted[1]
mrna$gross_ret <- mrna$MRNA.Adjusted/mrna$MRNA.Adjusted[1]
nflx$gross_ret <- nflx$NFLX.Adjusted/nflx$NFLX.Adjusted[1]

plot(log(gross_ret)~as.Date(row.names(djia)),data=djia,type='l',main="Gross Returns",xlab="Date",ylab="Log Gross Returns",ylim=c(0,5),las=1)
lines(log(gross_ret)~as.Date(row.names(sp500)),data=sp500,type='l',col="red")
lines(log(gross_ret)~as.Date(row.names(mrna)),data=mrna,type='l',col="green")
lines(log(gross_ret)~as.Date(row.names(nflx)),data=nflx,type='l',col="blue")
legend("bottomright", legend = c("DJIA", "SP500","MRNA","NFLX"), col=c("black","red","green","blue"), 
lty=c("solid","dashed","dashed","dashed"), bty="n")

# daily VaR at 1%
quantile(dji_daily_returns,probs=0.01,na.rm=TRUE) #-3.56%
quantile(sp500_daily_returns,probs=0.01,na.rm=TRUE) # -3.59%
quantile(mrna_daily_returns,probs=0.01,na.rm=TRUE)# -11.2%
quantile(nflx_daily_returns,probs=0.01,na.rm=TRUE) # -6.9%
qnorm(0.01,mean=mean(dji_daily_returns,na.rm=TRUE),sd=sd(dji_daily_returns,na.rm=TRUE)) #-0.03 = -3%
qnorm(0.01,mean=mean(sp500_daily_returns,na.rm=TRUE),sd=sd(sp500_daily_returns,na.rm=TRUE)) # -0.03 = -3%
qnorm(0.01,mean=mean(mrna_daily_returns,na.rm=TRUE),sd=sd(mrna_daily_returns,na.rm=TRUE)) # -0.11 = -11%
qnorm(0.01,mean=mean(nflx_daily_returns,na.rm=TRUE),sd=sd(nflx_daily_returns,na.rm=TRUE)) # -0.06 = -6%
# pretty close to the normal assumption

# monthly VaR at 1%
quantile(dji_monthly_returns,probs=0.01,na.rm=TRUE) # -11.2%
quantile(sp500_monthly_returns,probs=0.01,na.rm=TRUE) # -10.4%
quantile(mrna_monthly_returns,probs=0.01,na.rm=TRUE) # -33.9%
quantile(nflx_monthly_returns,probs=0.01,na.rm=TRUE) # -35.5%
qnorm(0.01,mean=mean(dji_monthly_returns,na.rm=TRUE),sd=sd(dji_monthly_returns,na.rm=TRUE)) # -0.109 = -10.9%
qnorm(0.01,mean=mean(sp500_monthly_returns,na.rm=TRUE),sd=sd(sp500_monthly_returns,na.rm=TRUE)) # -0.106 = -10.6%
qnorm(0.01,mean=mean(mrna_monthly_returns,na.rm=TRUE),sd=sd(mrna_monthly_returns,na.rm=TRUE)) # -0.555 = -55.5%
qnorm(0.01,mean=mean(nflx_monthly_returns,na.rm=TRUE),sd=sd(nflx_monthly_returns,na.rm=TRUE)) # -0.245 = -24.5%
# for the indices, the true quantiles are fairly close to the normal, however MRNA and NFLX are off by a large margin from the normal

# ------------------------ Question 2: T Testing --------------------------
