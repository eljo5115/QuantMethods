## --------------------------- ##
## Election Polarization Data   ##
## Author: Tony Cookson         ##
## ---------------------------- ##

## https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/42MVDX
setwd("/Users/eli/noticloud/Documents/GitHub/QuantMethods/Quant/data")
load("county_level_election_results.RData")

## Inspect data
head(dat)

## How has democratic vote shifted from 2012 to 2016?
## Scatterplot
plot(demvote_2016~demvote_2012, data=dat, pch= "+", bty="n", xlab = "% Voted Democrat in 2012", 
     ylab = c("% Voted Democrat in 2016"), xlim=c(0, 100), ylim=c(0,100), col="gray")
abline(lm(demvote_2016~demvote_2012, data=dat), lty="dashed", col="blue", lwd=1.5)

## How to estimate a regression in R
mylm <- lm(demvote_2016~demvote_2012, data=dat)
summary(mylm)
options(scipen = 16)   ## gets rid of scientific notation (out to 16 digits)
# options(scipen = 0)  ## puts it back.
## Spend some time on this:
## Interpretations
## slope vs. correlation
## how to compute correlation from output.
## hypothesis tests for slope and intercept.
##   --> Note: the default Ho: coef = 0 (regardless of coefficient)

## How to estimate a regression in R with scale()
mylm_s <- lm(demvote_2016~scale(demvote_2012), data=dat)
summary(mylm_s)

plot(demvote_2016~scale(demvote_2012), data=dat, pch= "+", bty="n",xlim=c((0-mean(dat$demvote_2012))/sd(dat$demvote_2012), (100-mean(dat$demvote_2012))/sd(dat$demvote_2012)), ylim=c(0,100), xlab = "SD from mean Voted Democrat in 2012", 
     ylab = c("% Voted Democrat in 2016"), col="gray")
abline(mylm_s, lty="dashed", col="blue", lwd=1.5)
# avg of 0, SD of 1 after scaling




## Longer term trend?
## How has democratic vote shifted from 2004 to 2016?
plot(demvote_2016~demvote_2004, data=dat, pch= "+", bty="n", xlab = "SD from mean Voted Democrat in 2004", 
     ylab = c("% Voted Democrat in 2016"), xlim=c(0, 100), ylim=c(0,100), col="gray")
abline(lm(demvote_2016~demvote_2004, data=dat), lty="dashed", col="brown", lwd=1)

## regression based on vote shares in 2004.
mylm2 <-lm(demvote_2016~demvote_2004, data=dat)
summary(mylm2)

## Ho: beta = 1 (beta =1 is the hypothesis of "noisy persistence": a
##     one pct increase in dem support in 2000 predicts a one pct increase in
##     dem support in 2016)
## Ha: beta !=1 (beta < 1 means "reversion to the mean" a one pct
##     increase in dem support in 2000 predicts a less than one pct 
##     increase in dem support in 2016; beta > 1 ==> more support for dem districts
##     than in the past.)

test_stat <- (1.03149 - 1)/0.01179
test_stat
2*pt(-abs(test_stat), df=3149)

## Reject the null hypothesis that the true beta equals 1.


## Split the analysis by democrat and republican districts based on 2000 vote
plot(demvote_2016~demvote_2004, data=dat[dat$dem_district2000==0,], pch= "+", bty="n", xlab = "% Voted Democrat in 2004", 
     ylab = c("% Voted Democrat in 2016"), xlim=c(0, 100), ylim=c(0,100), col="pink", cex=0.8)
points(demvote_2016~demvote_2004, data=dat[dat$dem_district2000==1,], pch= "x", col="lightblue", cex=0.8)

dem_lm <- lm(demvote_2016~demvote_2004, data=dat[dat$dem_district2000==1,])
summary(dem_lm)

rep_lm <- lm(demvote_2016~demvote_2004, data=dat[dat$dem_district2000==0,])
summary(rep_lm)

## For dem districts, more democratic in 2004 predicts ===> **even more** dem support in 2016
## For rep districts, more dem


abline(rep_lm, col="red", lty="dotted", lwd=2)
abline(dem_lm, col="blue", lty="dotted", lwd=2)
