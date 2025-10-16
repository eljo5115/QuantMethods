## ---------------------------- ##
## Plotting historical returns  ##
## ---------------------------- ##

## Read in Ken French's data on historical returns ##
setwd("/Users/eli/noticloud/Documents/GitHub/QuantMethods/Investments")
ff = read.table("french_portfolios_2025.txt")
names(ff) = c("DATE","S.L", "S.M", "S.H", "B.L", "B.M", "B.H")

ff$DATE <- as.character(ff$DATE)
ff$DATE <- paste(ff$DATE, "01", sep = "")
## this function computes gross returns, given percentage returns as an input ##
gross_ret = function(vec){
  gross_ret = 1+(vec/100)
  return(gross_ret)
}

## Defining variables for gross returns ##
ff$big_low = gross_ret(ff$B.L)
ff$big_mid = gross_ret(ff$B.M)
ff$big_hi = gross_ret(ff$B.H)

ff$sm_low = gross_ret(ff$S.L)
ff$sm_mid = gross_ret(ff$S.M)
ff$sm_hi = gross_ret(ff$S.H)

## Reformatting the DATE variable as a date object ##
ff$DATE = as.Date(ff$DATE, format="%Y%m%d")

## Plotting historical returns ##
## Note: cumulative return is (1+r1)*(1+r2)*...*(1+rt), computed using cumprod(gross_return)

## Illustrating geomean calculation ##
plot(ff$DATE, cumprod(ff$big_hi), type="l", main="Large Firm Returns", sub="Black is high B/M, Blue is low B/M", xlab="Date", ylab="Cumulative Return")

gm_mean = function(a){prod(a)^(1/length(a))}

big_hi_gm = gm_mean(ff$big_hi)
ff$big_hi_gm = big_hi_gm
lines(ff$DATE, cumprod(ff$big_hi_gm), lty="dashed")


## Contrasting the historical returns from four types of portfolios ##
par(mfrow=c(1,2))
plot(ff$DATE, cumprod(ff$big_hi), type="l", main="Large Firm Returns", sub="Black is high B/M, Blue is low B/M", xlab="Date", ylab="Cumulative Return")
lines(ff$DATE, cumprod(ff$big_low), col="blue")

plot(ff$DATE, cumprod(ff$sm_hi), type="l", main="Small Firm Returns", sub="Black is high B/M, Blue is low B/M", xlab="Date", ylab="Cumulative Return")
lines(ff$DATE, cumprod(ff$sm_low), col="blue", lty="dashed")

