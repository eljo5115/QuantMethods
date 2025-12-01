## -------------------- ##
## Quantitative Methods ##
## Project: Blog Fight! ##
## Author: Buckeyes     ##
## -------------------- ##

## --------- ##
## Part 1: Construct the data set used for the first scatterplot.   
## --------- ##

library(quantmod) ## We have to load this library every time we use quantmod(). 
getSymbols("UNRATE", src = "FRED") ## Here is unemployment data from FRED.  We have to convert this to quarterly. 
getSymbols("A822RE1Q156NBEA", src = "FRED") ## This is quarterly government spending as a percent of GDP. 

## The following is our conversion from monthly to quarterly data, with help from Chat. 
ep <- endpoints(UNRATE, on = "quarters") ## This gives us the index where each quarter ends in the time series. 
unemp <- UNRATE[ep[-1] - 2] ## The first command drops the 0 row index and then we subtract 1 to get to the first month of the quarter, which is how the government spending data is reported. 

## We need a "Date" column within each data frame to merge using as.Date(). First, we need to make each data frames.
unemp <- as.data.frame(unemp)
spend <- as.data.frame(A822RE1Q156NBEA)

## Now, we can add the date column.  We also will rename our spend data frame column. 
unemp$Date <- as.Date(rownames(unemp))
spend$Date <- as.Date(rownames(spend))

names(spend) <- c("GOV", "Date")

## Now, let's merge in the interest of plotting. 

dat <- merge(unemp, spend, by ="Date", all = TRUE) ## I am keeping all of the data for now. 

## --------- ##
## Part 2: Reproduce Taylor's other scatterplot.   
## --------- ##

## Now, I am creating a dataset that is only from Q1 1990 to Q3 2010.  This is for Taylor's scatterplot. 

dat_1 <- dat[dat$Date >= as.Date("1990-01-01") & 
                         dat$Date <= as.Date("2010-07-01"), ]

## Here is Taylor's scatterplot!! It looks pretty consistent. 
plot(UNRATE~GOV, data = dat_1, xlab = "Government Purchases as a Percent of GDP", 
     ylab = "Unemployment Rate", xlim = c(17, 22), ylim = c(3,11), col = "blue", pch = 16)


## --------- ##
## Part 3: Compare the scatter plot you obtain using the full data set to the one that Taylor reports.
## --------- ##
