## ---------------------------- ##
## Week 14 Script               ##
## Author: Tony Cookson         ##
## ---------------------------- ##
setwd("/Users/eli/noticloud/Documents/GitHub/QuantMethods/Quant/data")
yt_pan <- read.csv("YouTubePanelData.csv")
yt_pan$Date <- as.Date(yt_pan$Date, format="%m/%d/%y")

yt_pan$pre_election <- as.numeric(yt_pan$Date < as.Date("2012-11-06"))

## ------------------------------------------------------------ ##
## Segment 1: Goal -- understand interactions, practice F tests ##
## ------------------------------------------------------------ ##

## Let's analyze the YouTube "Revenue Function"
mylm <- lm(Earnings~scale(Views), data=yt_pan)  ## warm up
summary(mylm)

## Was revenue different before versus after the election?
mylm2 <- lm(Earnings~ pre_election, data=yt_pan)  ## still warming up
summary(mylm2)

## Was revenue different before versus after the election, holding constant Views?
mylm3 <- lm(Earnings~ scale(Views)+pre_election, data=yt_pan)  ## multiple regression, holding constant
summary(mylm3)

## Was payment per view different before versus after the election?
mylm4 <- lm(Earnings~ pre_election*scale(Views), data=yt_pan)  ## interactions!
summary(mylm4)

## In this full model, does the inclusion of terms with pre_election matter?
summary(mylm)     ## no pre_election terms  <- "restricted" model
summary(mylm4)    ## both pre_election terms <- "unrestricted" model

anova(mylm, mylm4)  ## can reject the joint null hypothesis that beta2=0 and beta3=0

## ------------------------------ ##
## Segment 2: Grok Fixed Effects  ##
##   felm + some clustering       ##
## ------------------------------ ##

## Does the "thrust" of this analysis hold within video?
## include videoid FE

mylm5 <- lm(Earnings~ videoid+scale(Views)*pre_election, data=yt_pan)
summary(mylm5)   ## a dummy variable for each video ... "holds constant" the video.

library(lfe)
myfelm <- felm(Earnings~ scale(Views)*pre_election|videoid|0|0, data=yt_pan)  ## identical to mylm5, but "absorbs" the fixed effects
summary(myfelm)

myfelm2 <- felm(Earnings~ scale(Views)*pre_election|videoid|0|videoid, data=yt_pan)  ## clusters by videoid
summary(myfelm2)  ## same estimates as myfelm, but more conservative standard errors (relaxes independence)

myfelm3 <- felm(Earnings~ scale(Views)*pre_election|videoid|0|videoid+Date, data=yt_pan)  ## clusters by videoid AND Date
summary(myfelm3)  ## same estimates as myfelm, but more conservative standard errors (relaxes independence a little more)

myfelm4 <- felm(Earnings~ scale(Views)*pre_election|videoid+Date|0|videoid+Date, data=yt_pan)  ## + Date fixed effects
summary(myfelm4)  ## different estimates (bc of different fixed effects)
                  ## Note: can't estimate diff intercept for pre- vs post-election bc of Date fixed effects.

## What's going on here?  Let me tell you from experience.

## ------------------------------ ##
## Segment 3: Multiple Categories ##
##   and interactions             ## 
## ------------------------------ ##

## Note: this is like a decade interaction.

yt_pan$month <- format(yt_pan$Date, format="%b")

## Let's analyze the YouTube "Revenue Function"
mylm <- lm(Earnings~scale(Views), data=yt_pan)  ## warm up
summary(mylm)

## Was revenue different by month?
mylm2 <- lm(Earnings~ month, data=yt_pan)  ## "Sep" "Oct" "Nov" "Dec" "Jan" "Feb" "Mar"
summary(mylm2)  ## "omitted category" is Dec

## Was revenue different by month, holding constant Views?
mylm3 <- lm(Earnings~ scale(Views)+month, data=yt_pan)  ## multiple regression, holding constant
summary(mylm3)  ## this is a "month" fixed effect

## Was payment per view different before versus after the election?
mylm4 <- lm(Earnings~ scale(Views)*month, data=yt_pan)  ## interactions!
summary(mylm4)  ## is payment per view different by month?

## Several joint tests we could consider:

## 1: does monthly timing matter for average earnings?
summary(mylm2)   ## tests each month versus the baseline month (Dec)
anova(mylm2)     ## tests whether all months have the same mean  (joint test, F-test; uses the factor structure to get the F-stat)

## Note: the F-statistic in summary( ) happens to be the same here.
##       this F-stat is always the test for whether **all** slope coefs are zero.  Same as anova here.

## 2: does monthly timing matter for average earnings, holding constant views?
summary(mylm3)   ## tests each month versus the baseline month (Dec)
anova(mylm3)     ## tests whether all months have the same mean  (joint test, F-test; uses the factor structure to get the F-stat)

## Note: separate F-tests for separate factors in anova.  The "omnibus" F-stat from summary() isn't the same bc it also tests coef on views =0
anova(mylm, mylm3)   ## Restricted versus unrestricted implementation  --> same F-stat as anova(mylm3)

## 3: are slopes by month all the same?
summary(mylm4)  ## are scale(Views):month* coefs jointly equal to zero?
anova(mylm4)    ## anova does this for us! (F-stat of 35.76)

anova(mylm3, mylm4) ## can also do restricted vs unrestricted, same F-stat (can be useful if coefs aren't all bundled into one factor)
