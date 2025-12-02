### ------------------------- ## ------------------------- ###
###    Blog Fight Project     ##        Preston Bell       ### 
###        Bears Group        ##         Jaden Carr        ###
###   Quantitative Methods    ##         Julia Cemin       ###
###     Professor Cookson     ##       Daniel Biernat      ###
### ------------------------- ## ------------------------- ###

setwd("~/Desktop/CU/Fall 2025 B/Quantitative Methods* (5031)/Week 14 - Final Project")
options(scipen = 999)

# ------------------------- #
#        Data Setup         #
# ------------------------- #

# imports data from CSV (downloaded from FRED)
gdp  <- read.csv("GDP.csv")    
gce  <- read.csv("GCE.csv") 
unem <- read.csv("UNRATE.csv")

# converts dates, adds label, aggregates GDP to quarterly average
gdp$Date <- as.Date(gdp$observation_date) 
gdp$Year <- format(gdp$Date, "%Y"); gdp$Qtr <- quarters(gdp$Date)
gdp$Quarter <- paste(gdp$Year, substr(gdp$Qtr,2,2), sep="Q")
gdp_q <- aggregate(GDP ~ Quarter, data=gdp, FUN=mean)

# converts dates, adds label, aggregates GCE to quarterly average
gce$Date <- as.Date(gce$observation_date) 
gce$Year <- format(gce$Date, "%Y"); gce$Qtr <- quarters(gce$Date)
gce$Quarter <- paste(gce$Year, substr(gce$Qtr,2,2), sep="Q")
gce_q <- aggregate(GCE ~ Quarter, data=gce, FUN=mean)

# converts dates, adds label, aggregates Unemployment to quarterly average
unem$Date <- as.Date(unem$observation_date) 
unem$Year <- format(unem$Date, "%Y"); unem$Qtr <- quarters(unem$Date)
unem$Quarter <- paste(unem$Year, substr(unem$Qtr,2,2), sep="Q")
unem_q <- aggregate(UNRATE ~ Quarter, data=unem, FUN=mean)

# merges data sets into single data frame
df <- merge(gdp_q, gce_q, by="Quarter")
df <- merge(df, unem_q, by="Quarter")

# calculates new variable for government share of GDP
df$GovShare <- df$GCE / df$GDP * 100

# ------------------------- #
#        Correlations       #
# ------------------------- #

# correlations by period
corr_pre    <- with(subset(df, Quarter < "1990Q1"), cor(GovShare, UNRATE))
corr_taylor <- with(subset(df, Quarter >= "1990Q1" & Quarter <= "2010Q3"), cor(GovShare, UNRATE))
corr_post   <- with(subset(df, Quarter >= "2010Q3"), cor(GovShare, UNRATE))
corr_pre; corr_taylor; corr_post

## Output:
# -0.2065
# -0.8239
# -0.9226

# creates new variable that defines time period
df$Period <- with(df,factor(ifelse(Quarter < "1990Q1", "Pre",
  ifelse(Quarter <= "2010Q3", "Taylor", "Post")), levels=c("Pre","Taylor","Post")))

# regression summary with interactions
model <- lm(UNRATE ~ GovShare * Period, data=df)
summary(model)

## Output:
# Coefficients:
#                         Estimate Std. Error t value             Pr(>|t|)    
# (Intercept)             9.68584    1.20752   8.021   0.0000000000000229 ***
# GovShare               -0.18501    0.05525  -3.348             0.000915 ***
# PeriodTaylor          -24.89168    2.85550  -8.717 < 0.0000000000000002 ***
# PeriodPost            -39.16915    3.41400 -11.473 < 0.0000000000000002 ***
# GovShare:PeriodTaylor   1.27156    0.14463   8.792 < 0.0000000000000002 ***
# GovShare:PeriodPost     2.13332    0.18575  11.485 < 0.0000000000000002 ***

# Multiple R-squared:  0.3959,	Adjusted R-squared:  0.386 
# F-statistic: 39.85 on 5 and 304 DF,  p-value: < 0.00000000000000022

# ------------------------- #
#        Scatterplots       #
# ------------------------- #

## Taylor period scatterplot

# isolates Taylor period data, creates scatterplot
taylor_data <- subset(df, Period == "Taylor")
plot(taylor_data$GovShare,
     taylor_data$UNRATE,
     xlab = "Gov Purchases as % of GDP",
     ylab = "Unemployment Rate %",
     main = "Unemployment vs. Gov Purchases (1990Q1–2010Q3)",
     pch = 18, 
     col = "blue")


## Full-sample scatterplot with groups

# breaks out data sets, creates empty plot with full axis ranges
pre_data    <- subset(df, Period == "Pre")
taylor_data <- subset(df, Period == "Taylor")
post_data   <- subset(df, Period == "Post")
x_range <- range(df$GovShare, na.rm = TRUE)
y_range <- range(df$UNRATE, na.rm = TRUE)

# adds labels to plot frame
plot(x_range, y_range,
     type = "n",          # "n" = no points yet, just frame
     xlab = "Gov Purchases as % of GDP",
     ylab = "Unemployment Rate %",
     main = "Unemployment vs. Gov Purchases")

# adds data points for each period to plot
points(pre_data$GovShare, pre_data$UNRATE,
       pch = 18,
       col = "red")           

points(taylor_data$GovShare, taylor_data$UNRATE,
       pch = 18,          
       col = "blue")

points(post_data$GovShare, post_data$UNRATE,
       pch = 18,          
       col = "green")

# adds legend to plot
legend("topleft",
       legend = c("Pre-1990", "1990Q1–2010Q3 (Taylor)", "2010Q4–Present"),
       pch = c(18, 18, 18),
       col = c("red", "blue", "green"),
       bty = "n")

# ------------------------- #
#        Monte Carlo        #
# ------------------------- #

# creates monte carlo simulation with a for loop
set.seed(5)
Nsim <- 1000;  T <- 256;  minT <- 60
maxCorr <- numeric(Nsim);  fullCorr <- numeric(Nsim)
for(i in 1:Nsim){
  x <- rnorm(T);  y <- rnorm(T)
  fullCorr[i] <- cor(x,y)
  cvals <- sapply(minT:T, function(L) cor(tail(x,L), tail(y,L)))
  maxCorr[i] <- max(cvals, na.rm=TRUE)
}

# summary stats for sample correlations
mean(fullCorr); sd(fullCorr)
mean(maxCorr); sd(maxCorr)
quantile(maxCorr, probs=c(0.9,0.95,0.99))
max(maxCorr)

## Output:
# mean(fullCorr) ≈ -0.002,   sd(fullCorr) ≈ 0.0621
# mean(maxCorr)  ≈ 0.0817,   sd(maxCorr)  ≈ 0.0907
# 90%  maxCorr ≈ 0.204,   95% maxCorr ≈ 0.243,   99% maxCorr ≈ 0.311
# max(maxCorr) ≈ 0.37




