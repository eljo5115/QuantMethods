# -----------
# Author: Eli Jordan
# Case 11, Active vs Passive managed ETFs
# -----------

if(!require(quantmod)){
  install.packages("quantmod")
}
library(quantmod)

# ---------------------------- Part I ----------------------------
stocksToAnalyze <- c("PRNHX","VMGIX","XMMO")
getSymbols(stocksToAnalyze,src="yahoo",from=as.Date("2019-06-30"),to=as.Date("2022-10-30"))

# construct returns data frame
# same date range, constructed from returns
dat <- cbind(Delt(PRNHX$PRNHX.Adjusted,k=1),Delt(VMGIX$VMGIX.Adjusted,k=1),Delt(XMMO$XMMO.Adjusted))
names(dat) <- c("ret_a","ret_p","ret_x")

# returns for PRNHX, active
hist(dat$ret_a,breaks=20,main="PRNHX Returns")
summary(dat$ret_a)
# returns for VMGIX, passive
hist(dat$ret_p,breaks=20,main="VMGIX Returns")
summary(dat$ret_p)
# returns for XMMO
hist(dat$ret_x,breaks=20,main="XMMO Returns")
summary(dat$ret_x)

# plot PRNHX to VMGIX
plot(ret_a~ret_p,data=dat,pch="+",ylab="PRNHX Returns",xlab="VMGIX Returns")
cor(dat$ret_a,dat$ret_p,use="complete.obs") #0.832
# With a correlation of 0.832, we are assuming that for each point that VMGIX moves
# PRNHX will move 0.832 points

char_lm <- lm(ret_a~ret_p,data=dat)
summary(char_lm)
# R-squared: 0.6929
# R-squared is the correlation squared
# Slope: 1.0094780
# alpha: 0.0095
# For every percent that VMGIX returns, then PRNHX should return 1.00947, on average
# Intercept: 0.0006015
# If VGMIX returns = 0, then PRNHX returns are predicted to be 0.0006015, 0.06%, 

# ---------------------------- Part II -----------------------------
# Intercept SE: 0.0004032
# Slope SE: 0.0232172
# This is the standard error for the sampling distribution that the intercept and slope are 
# derived from. Essentially the Slope and Intercept estimates are the sample mean

# Intercept t-value: 1.492
# Slope t-value: 43.48 (really big)
# Intercept H0: intercept=0
# this would mean that when the X value (VMGIX) is 0, PRNHX would also be 0. This makes sense because
# it assumes the slope (correlation) is 1. We can assume this isn't true as it usually isn't, however
# it's a good hypothesis to test.
# Slope H0: slope=1
# two-sided alternative: ret_p - slope < 0, the estimated slope is less than the H0
# Similar to the intercept, we assume the correlation is 1 to test if the correlation is 1. Again, it's
# very unlikely 2 assets will be perfectly correlated (unless they are the same) but it's a good hypothesis
# to see how far from 'expected' we end up.

# Slope H0: slope = 1 (The relationship between PRNHX and VMGIX returns is one-to-one)
# Slope H1: slope != 1 (The relationship is not one-to-one)

# Extract values from the model and calculate the t-statistic
model_summary <- summary(char_lm)
b1 <- model_summary$coefficients["ret_p", "Estimate"]      # Estimated slope
se_b1 <- model_summary$coefficients["ret_p", "Std. Error"] # Std. Error of slope

t_stat <- (b1 - 1) / se_b1
# t_stat = (1.0094780 - 1) / 0.0232172 = 0.4082

# Calculate the p-value
df <- model_summary$df[2] # Residual degrees of freedom (n-2), which is 838
p_val <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
# p_val = 0.6832

# With a significance level of alpha = 0.05, our p-value (0.683) is much larger.
# Therefore, we fail to reject the null hypothesis. There is not enough
# statistical evidence to say the true slope is different from 1.

# ---------------------------- Part III -----------------------------
mylm2 <- lm(ret_a~scale(ret_p), data=dat)
summary(mylm2)

# Scaled Intercept estimate: 0.0010070, Intercept Estimate: 0.0006015
# scaled intercept is larger
# Intercept is larger, scaling the X-axis makes the mean 0 and each tick a SD point.
# Therefore, the intercept at 0.001070 means that when VMGIX returns are 3 SD away, PRNHX returns
# are 0.0010070.

# Scaled Slope intercept: 0.0175, much lower than non-scaled
# For each SD VMGIX moves, PRNHX returns moves 0.0175 
# This isn't a great case for the Scale function as the SD of returns is always changing
# and it doesn't provide a great estimate of returns. also the slope (correlation) is much lower
# so a somewhat worse prediction

# R-squared does not change from non-scaled to scaled. also the Slope t-value doesn't change.
# this makes sense as the Residuals would be the same as technically the points are the same, just different units
# Slope t-value staying the same also makes sense because the R-squared is the same


# ---------------------------- Part IV -----------------------------
#plot XMMO returns to VMGIX returns
plot(ret_x~ret_p,data=dat,pch="+",ylab="XMMO Returns",xlab="VMGIX Returns")
cor(dat$ret_x,dat$ret_p,use="complete.obs") #Beta: 0.9089183

x_lm <- lm(ret_x~ret_p,data=dat)
x_sum <- summary(x_lm)

# Intercept: 0.001508, t-value: 0.6
# slope (Beta): 0.9134441, t-value: 63.1
# alpha: ~ -0.1

# Extract values from the model and calculate the t-statistic
x_b1 <- x_sum$coefficients["ret_p", "Estimate"]      # Estimated slope
x_se_b1 <- x_sum$coefficients["ret_p", "Std. Error"] # Std. Error of slope

x_t_stat <- (x_b1 - 1) / x_se_b1
# t_stat = (0.9134441 - 1) / 0.01447585 = -5.97933

# 3. Calculate the p-value
x_df <- model_summary$df[2] # Residual degrees of freedom (n-2), which is 838
x_p_val <- 2 * pt(abs(x_t_stat), df = x_df, lower.tail = FALSE)
# p_val = 3e-9

# XMMO's slope is less than PRNHX, so it is underperforming the VMGIX benchmark we have set. PRNHX
# was above 1 so PRNHX was outperforming VMGIX. This also means that XMMO has alpha: -0.1 and
# PRNHX's alpha: 0.0095. Clearly, PRNHX has the higher (positive) alpha so clearly the better choice.