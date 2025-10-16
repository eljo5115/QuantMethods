## -------------------------
## Case 1 - Intro to R
## Author: Eli Jordan
## MSBC 5031 - Quant Methods
## ------------------------- 


load('data/SP500.RData')
# Plot SP500 Gross YTD Returns
plot(x=sp500$Date, y=sp500$GrossReturn_YTD, type='l', main="S&P 500 YTD Returns")
# -----------------------------------
# Q1 Extract GrossReturn_YTD from ual
ual$GrossReturn_YTD
# -----------------------------------

# Plot UAL Close Prices
plot(x=ual$Date, y=ual$Adj.Close, type='l', main="UAL Price")
# Plot UAL, S&P YTD Return Difference
plot(x=sp500$Date, y=sp500$GrossReturn_YTD - ual$GrossReturn_YTD, type='l',main='UAL Relative to S&P')
# Plot UAL Gross YTD Returns
plot(GrossReturn_YTD~Date, data=ual,type='l',main='$UAL YTD Gross Returns')

# Q2 Early 2020 S&P Analysis
# Early (End Q1) 2020 was the start of the covid pandemic, impacting most of the stocks
# in the s&p index negatively causing a sharp drop

# difference between s&P and UAL
# creates new column in ual dataframe
ual$abn_gross_return <- ual$GrossReturn_YTD - sp500$GrossReturn_YTD
plot(abn_gross_return~Date, data=ual, type='l',main="UAL Difference between s&p")
#plot sp500 and ual gross returns
plot(GrossReturn_YTD~Date,data=sp500,type='l',col='red', main="S&P500 vs $UAL",ylim=c(0.2,1.3))
lines(GrossReturn_YTD~Date, data=ual, type='l',col='blue',lty='dashed')
# Q3 UAL vs Generic Market
# Looking at the above plot (Titled S&P500 vs $UAL) shows that UAL fared much worse than the 
# overall market. YTD returns dropped sharply around March and didn't recover very much before
# the end of the year


# lists for positive YTD Returns for sp500 and ual
sp500_isPositive <- sp500$GrossReturn_YTD > 1
ual_isPositive <- ual$GrossReturn_YTD > 1