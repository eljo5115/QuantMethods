## -------------------------
## Case 1 - Intro to R
## Author: Eli Jordan
## MSBC 5031 - Quant Methods
## ------------------------- 


load('data/SP500.RData')
# Plot SP500 Gross YTD Returns
plot(x=sp500$Date, y=sp500$GrossReturn_YTD, type='l', main="S&P 500 YTD Returns")
# Plot UAL Gross YTD Returns
plot(x=ual$Date, y=ual$Adj.Close, type='l', main="UAL Price")
# Plot UAL, S&P YTD Return Difference
plot(x=sp500$Date, y=sp500$GrossReturn_YTD - ual$GrossReturn_YTD, type='l',main='UAL Relative to S&P')

# difference between s&P and UAL
# creates new column in ual dataframe
ual$abn_gross_return <- ual$GrossReturn_YTD - sp500$GrossReturn_YTD

#plot sp500 and ual gross returns
plot(GrossReturn_YTD~Date,data=sp500,type='l',col='red', main="S&P500 vs $UAL")
lines(GrossReturn_YTD~Date, data=ual, type='l',col='blue',lty='dashed')


# lists for positive YTD Returns for sp500 and ual
sp500_isPositive <- sp500$GrossReturn_YTD > 1
ual_isPositive <- ual$GrossReturn_YTD > 1