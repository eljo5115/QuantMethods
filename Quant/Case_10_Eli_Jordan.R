# ----------
# Author: Eli Jordan
# Week 10 Case 
# ----------

if (!require("quantmod")) {
  install.packages("quantmod")
}
library(quantmod)
# function called a the bottom to run for months 60 and 36
#--------------------- Part 1 -------------------------
case10 <- function(months){
  stocks_to_analyze <- c("SPY","TSLA","GE")
  num_months <- months + 1
  getSymbols(stocks_to_analyze,src="yahoo",from=Sys.Date()-num_months*30,to=Sys.Date(),periodicity = "monthly")

  spy <- as.data.frame(SPY$SPY.Adjusted)
  tsla <- as.data.frame(TSLA$TSLA.Adjusted)
  ge <- as.data.frame(GE$GE.Adjusted)

  spy$spy_ret <- Delt(spy$SPY.Adjusted,k=1)
  tsla$tsla_ret <- Delt(tsla$TSLA.Adjusted,k=1)
  ge$ge_ret <- Delt(ge$GE.Adjusted,k=1)

  spy$spy_grossret <- spy$SPY.Adjusted/spy$SPY.Adjusted[1]
  tsla$tsla_grossret <- tsla$TSLA.Adjusted/tsla$TSLA.Adjusted[1]
  ge$ge_grossret <- ge$GE.Adjusted/ge$GE.Adjusted[1]

  spy$Date <- row.names(spy)
  tsla$Date <- row.names(tsla)
  ge$Date <- row.names(ge)

  names(spy) <- c("spy_price","spy_ret","spy_grossret","Date")
  names(tsla) <- c("tsla_price","tsla_ret","tsla_grossret","Date")
  names(ge) <- c("ge_price","ge_ret","ge_grossret","Date")

  merged_df <- merge(spy,tsla,by="Date")
  merged_df <- merge(merged_df,ge,by="Date")

  #--------------------- Part 2 --------------------------

  # --------------- A -------------

  # expectedly low average return
  spy_er <- mean(merged_df$spy_ret,na.rm=TRUE) # 0.0145 = 1.45%
  # High volatility can result in higher than normal returns
  tsla_er <- mean(merged_df$tsla_ret,na.rm=TRUE) # 0.036 = 3.64%
  # low volatility, yet high returns, thumbs up
  ge_er <- mean(merged_df$ge_ret,na.rm=TRUE) # 0.04133 = 4.13%


  # low vol
  spy_sd <- sd(merged_df$spy_ret,na.rm=TRUE) # 0.0459
  # Tesla has super high volatility
  tsla_sd <- sd(merged_df$tsla_ret,na.rm=TRUE) # 0.18355
  # low vol
  ge_sd <- sd(merged_df$ge_ret,na.rm=TRUE) # 0.04134

  # ------------- B -----------------

  # As expected, SPY has the most normal histogram
  hist(merged_df$spy_ret, main="SPY Returns",xlab="Decimal Returns",breaks=20)
  abline(v=spy_er,col="red",lty=2)
  # While Tesla is thicker in the middle
  hist(merged_df$tsla_ret, main="TSLA Returns",xlab="Decimal Returns",breaks=20)
  abline(v=tsla_er,col="red",lty=2)
  # and GE is relatively normal
  hist(merged_df$ge_ret, main="GE Returns",xlab="Decimal Returns",breaks=20)
  abline(v=ge_er,col="red",lty=2)

  # -------------- TSLA ~ SPY ------------------
  # The scatter plot shows a positive correlation between SPY and Tesla
  # As the general trend is up and to the right
  plot(merged_df$tsla_ret~merged_df$spy_ret
  ,main="Tesla to SPY"
  ,xlab="SPY Returns"
  ,ylab="Tesla Returns"
  )
  # The correlation is the slope of the best fit line 
  tsla_cor <- cor(merged_df$spy_ret,merged_df$tsla_ret,use="complete.obs") #0.5022

  # -------------- GE ~ SPY ------------------
  # This plost shows a much tighter correlation between GE and SPY than
  # we saw with Tesla.
  # The trend is still positive as it seems to be up and to the right
  plot(merged_df$ge_ret~merged_df$spy_ret
  ,main="GE to SPY"
  ,xlab="SPY Returns"
  ,ylab="GE Returns"
  )
  # The tighter positive correlation can be shown with cor() giving a number
  # closer to 1 than the Tesla case
  ge_cor <- cor(merged_df$spy_ret,merged_df$ge_ret,use="complete.obs") #0.7037

  #------------------ GE ~ TSLA ------------------
  # This plot looks almost like 0 correlation however, there is still a few
  # points that have an upward trend
  plot(merged_df$ge_ret~merged_df$tsla_ret
  ,main="GE to TSLA"
  ,xlab="TSLA Returns"
  ,ylab="GE Returns"
  )
  # From the chart we could expect a lower positive correlation than before
  # and the number proves that. 0.347 is a fairly low correlation number.
  cor(merged_df$tsla_ret,merged_df$ge_ret,use="complete.obs") #0.347

  # ------------- C ----------------
  # slope = Cov[X,Y]/Var[X]
  # intercept = Y - slope*X

  # Tesla ~ SPY
  slope_tesla <- tsla_cor * (tsla_sd/spy_sd) #2.01
  intercept_tesla <- tsla_er - slope_tesla*spy_er # 0.0073
  # therefore line equation: y = slope_tesla*x + intercept_tesla

  # GE ~ SPY
  slope_ge = ge_cor * (ge_sd/spy_sd) # 1.57
  intercept_ge = ge_er - slope_ge*spy_er # 0.0186
  # therefore line equation: y= slope_ge*x + intercept_ge


  # ------------- D -----------------
  tsla_lm <- lm(merged_df$tsla_ret~merged_df$spy_ret) # 0.0073, 2.006
  ge_lm <- lm(merged_df$ge_ret~merged_df$spy_ret) # 0.0186, 1.57
  # lm() confirms that the by-hand approach is accurate


  # ------------- E -----------------
  # Plot Tesla
  plot(merged_df$tsla_ret~merged_df$spy_ret
  ,main="Tesla to SPY w/ bf line"
  ,xlab="SPY Returns"
  ,ylab="Tesla Returns"
  )
  abline(a=intercept_tesla,b=slope_tesla,col="red",lty=2)
  abline(v=mean(merged_df$spy_ret,na.rm=TRUE),col="blue",lty=2)
  abline(h=mean(merged_df$tsla_ret,na.rm=TRUE),col="blue",lty=2)
  # Plot GE
  plot(merged_df$ge_ret~merged_df$spy_ret
  ,main="GE to SPY w/ bf line"
  ,xlab="SPY Returns"
  ,ylab="GE Returns"
  )
  abline(a=intercept_ge,b=slope_ge,col="red",lty=2)
  abline(v=mean(merged_df$spy_ret,na.rm=TRUE),col="blue",lty=2)
  abline(h=mean(merged_df$ge_ret,na.rm=TRUE),col="blue",lty=2)

  # Yes, both of the lines on the graphs meet at the regression line. Yes this has to happen
  # due to the way the intercept is calculated it guarantees that the means are on the 
  # regression line

  # ------------- G ---------------
  summary(tsla_lm)
  # r-squared_tsla = 0.2522

  summary(ge_lm)
  # r-squared_ge = 0.4953

  # r-squared is the variance in the correlated data set
  # a measure of how predictive the model is
  # so an r-squared value of 0.2522 (tesla) is only somewhat predictive
  # and 0.50 (ge) is more predictive that tesla

  merged_df$resids_tsla <- c(NA, residuals(tsla_lm))
  merged_df$resids_ge <- c(NA, residuals(ge_lm))

  mean(merged_df$resids_tsla,na.rm=TRUE) #-7.86e-18
  mean(merged_df$resids_ge,na.rm=TRUE)# 2.312e-18

  cor(merged_df$resids_tsla,merged_df$spy_ret,use="complete.obs") # 8e-17
  cor(merged_df$resids_ge,merged_df$spy_ret,use="complete.obs") # 1e-18

# residuals are basically not correlated to SPY returns
}

# run case10 function by inputting number of months of data to analyze
case10(60)
# do it again for 36 months
case10(36)

#TSLA beta 2.08
# GE beta 1.49
# this roughly aligns with what our calculations led to
# There is some slight variation but we can assume this is from daily returns data
# opposed to monthly returns
