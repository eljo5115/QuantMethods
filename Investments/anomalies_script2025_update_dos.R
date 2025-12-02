### Read in Fama French Factors ##
setwd("/Users/eli/noticloud/Documents/GitHub/QuantMethods/Investments/data")
ff = read.csv("F-F_Research_Data_Factors2025.csv")   ## Monthly
ff$Date = as.Date(paste(ff$Date,"01",sep=""), format="%Y%m%d")
ff$month = format(ff$Date, format="%m")

boxplot(Mkt.RF~month, data=ff)
boxplot(HML~month, data=ff)
boxplot(SMB~month, data=ff)

## Showing the January Effect
summary(lm(Mkt.RF~month, data=ff))
summary(lm(Mkt.RF~I(month=="01"), data=ff))
summary(lm(HML~month, data=ff))
summary(lm(HML~I(month=="01"), data=ff))
summary(lm(SMB~month, data=ff))
summary(lm(SMB~I(month=="01"), data=ff))

## Before 1980 ##
summary(lm(Mkt.RF~month, data=ff[ff$Date<as.Date("1977-01-01"),]))
summary(lm(Mkt.RF~I(month=="01"), data=ff[ff$Date<as.Date("1977-01-01"),]))

summary(lm(HML~month, data=ff[ff$Date<as.Date("1977-01-01"),]))
summary(lm(HML~I(month=="01"), data=ff[ff$Date<as.Date("1977-01-01"),]))

summary(lm(SMB~month, data=ff[ff$Date<as.Date("1977-01-01"),]))
summary(lm(SMB~I(month=="01"), data=ff[ff$Date<as.Date("1977-01-01"),]))

## Checking it in recent years
summary(lm(Mkt.RF~month, data=ff[ff$Date>as.Date("1976-01-01"),]))
summary(lm(Mkt.RF~I(month=="01"), data=ff[ff$Date>as.Date("1976-01-01"),]))

summary(lm(HML~month, data=ff[ff$Date>as.Date("1976-01-01"),]))
summary(lm(HML~I(month=="01"), data=ff[ff$Date>as.Date("1976-01-01"),]))

summary(lm(SMB~month, data=ff[ff$Date>as.Date("1976-01-01"),]))
summary(lm(SMB~I(month=="01"), data=ff[ff$Date>as.Date("1976-01-01"),]))


summary(lm(SMB~I(month=="01")*I(ff$Date>as.Date("1976-12-01")), data=ff))


## Daily returns for day-of-week effects
ffd = read.csv("F-F_Research_Data_Factors_daily2025.csv")
ffd$Date = as.Date(as.character(ffd$Date), format="%Y%m%d")
ffd$day_of_week = weekdays(ffd$Date)


boxplot(Mkt.RF~day_of_week, data=ffd)

## Showing the Monday (Weekend) Effect
summary(lm(Mkt.RF~day_of_week, data=ffd))

## Checking it after French noticed it...
summary(lm(Mkt.RF~day_of_week, data=ffd[ffd$Date>as.Date("1981-01-01"),]))
