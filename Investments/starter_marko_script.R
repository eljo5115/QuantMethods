## 
setwd("/Users/eli/noticloud/Documents/GitHub/QuantMethods/Investments")
underlying_data <- read.csv("MarkoModelData_InClass.csv")
retz <- underlying_data[,c("SPYret", "MMMret", "Aret", "Fret")]
cor(retz, use="complete.obs")

# we need SD(F) and E[F]

f_sd <- sd(retz$Fret,na.rm=TRUE)
f_er <- mean(retz$Fret,na.rm=TRUE)

if( !require('quantmod')){
  install.packages('quantmod')
}
library(quantmod)

getSymbols(c("A","^GSPC","F","MMM"), src = "yahoo", from = "2017-01-01", to = Sys.Date())

a_df <- as.data.frame(A)
f_df <- as.data.frame(F)
mmm_df <- as.data.frame(MMM)
sp500_df <- as.data.frame(GSPC)

a_monthly <- to.monthly(A,indexAt = "lastof", OHLC = TRUE)
f_monthly <- to.monthly(F,indexAt = "lastof", OHLC = TRUE)
mmm_monthly <- to.monthly(MMM,indexAt = "lastof", OHLC = TRUE)
sp500_monthly <- to.monthly(GSPC,indexAt = "lastof", OHLC = TRUE)

a_monthly_adjusted <- to.monthly(A[,"A.Adjusted"], indexAt = "lastof", OHLC = FALSE)
f_monthly_adjusted <- to.monthly(F[,"F.Adjusted"], indexAt = "lastof", OHLC = FALSE)
mmm_monthly_adjusted <- to.monthly(MMM[,"MMM.Adjusted"], indexAt = "lastof", OHLC = FALSE)
sp500_monthly_adjusted <- to.monthly(GSPC[,"GSPC.Adjusted"], indexAt = "lastof", OHLC = FALSE)

a_monthly_returns <- Delt(a_monthly_adjusted, k = 1)
f_monthly_returns <- Delt(f_monthly_adjusted, k = 1)
mmm_monthly_returns <- Delt(mmm_monthly_adjusted, k = 1)
sp500_monthly_returns <- Delt(sp500_monthly_adjusted, k = 1)

ret_df <- data.frame(a_monthly_returns,f_monthly_returns,mmm_monthly_returns,sp500_monthly_returns)
names(ret_df) <- c("Aret","Fret","MMMret","SPYret")
# create correlation matrix
cor(ret_df,use="complete.obs")
# create covariance matrix
cov(ret_df,use="complete.obs")
