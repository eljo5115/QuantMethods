## ---------------------------------- ##
## Hypothesis Testing Script          ##
## Author: Tony Cookson               ##
## ---------------------------------- ##

## Statistics objectives:
## 1) Conceptual hypothesis testing -- showing the "nuts and bolts"
##    of a one-sample t-test.
## 2) Using canned functions to perform one-sample t-test.

## set working directory
# setwd("E:/Dropbox/Teaching/Business Stat Class/Fall 2020/Code/Class")
source("plot_sampd.R")
source("draw_samp_dist.R")

## ------------------------------------ ##
## Block 1: Hypothesis testing by hand  ##
## ------------------------------------ ##

## Suppose we obtain a sample and compute a sample mean (X_BAR) of 10 ## 
## How unlikely is that?

mu_null = 5
X_BAR   = 10
se_null = 10/sqrt(10)

abline(v=X_BAR, col="red")
abline(h=0, lty="dashed")

## "One Sided" p-value
pnorm(X_BAR, mean = mu_null, sd = se_null, lower.tail = FALSE) ## area to the right of red vertical line
pnorm(X_BAR, mean = mu_null, sd = se_null) ## area to the left of red vertical line

abline(v=0, col="red")

## "Two Sided" p-value assuming normal
2*pnorm(X_BAR, mean = mu_null, sd = se_null, lower.tail = FALSE) ## area to the right of red vertical line

## --------------------------------------------------------------- ##
## This was fine, but try a standardized way to look at it         ##
## Problem with above: se_xbar != sd_xbar                          ##
## --- aside from how to plot N(0,1) and t(df), the following      ##
## --- block of code shows how these two differ for a small sample ##
## --- size.  Here: N=10                                           ##
## --------------------------------------------------------------- ##

test_stat = (X_BAR - mu_null)/se_null

## The test_stat has a t(df) distribution, which is *approximately* normal ##

## Plot the standard normal distribution.
x = (-3500:3500)/1000         ## grid points
d = dnorm(x, mean=0, sd = 1)  ## density values at each grid point
plot(x, d, type="l", main = "N(0,1) versus t(df=9)", 
     xlab = "T or Z-Scores", ylab= "density")
abline(h=0, lty = "dashed")
abline(v=test_stat, col="blue")

## By comparison, plot the t distribution with 9 df (10 observation minus 1)
dens_t = dt(x, df=9)
lines(x, dens_t, lty="dashed", col="red")

## "One sided" p-value ##
pnorm(test_stat, lower.tail=FALSE)
pt(test_stat, df= 9, lower.tail=FALSE)

## "Two sided" p-value ##
abline(v=-test_stat, col="blue")
2*pnorm(test_stat, lower.tail=FALSE)
2*pt(test_stat, df= 9, lower.tail=FALSE)

## What if we "assumed" the wrong value for the mean, say mu_null = 0 

mu_null = 0
X_BAR   = 10
se_null = 10/sqrt(10)

test_stat2 = (X_BAR- mu_null)/se_null

abline(v=test_stat2, col="blue", lty="dashed")
abline(v=-test_stat2, col="blue", lty="dashed")

pnorm(test_stat2, lower.tail=FALSE)   ## one-sided (probability to the right of the blue dashed line)
2*pnorm(test_stat2, lower.tail=FALSE) ## two-sided (probability outside of blue dashed lines)

pt(test_stat2, df=9, lower.tail=FALSE)
2*pt(test_stat2, df=9, lower.tail=FALSE)  ## different from above.  Even a different conclusion if the "cutoff" is 1%

## ----------------------------- ##
## Now, consider a larger sample ##
## N = 50                        ##
## ----------------------------- ##
set.seed(1)  ## same page?
x_sample = draw_sample_somehow(50)  ## Actually, start with an actual sample of observations...
x_bar   = mean(x_sample)
std_err = sd(x_sample)/sqrt(50)
x_bar

## Is this statistically different from mu = 5? ##
test_stat = (x_bar - 5)/std_err

2*pnorm(abs(test_stat), lower.tail=FALSE)   ## abs() is the absolute value, calculation only works in general like this

## The technically "right" way to do this. test_stat isn't *exactly* normal.
2*pt(abs(test_stat), df = 49, lower.tail=FALSE)   ## abs() is the absolute value, calculation only works in general like this


## --------------------------------------------##
## Block 2: Using canned functions and forming ##
## confidence intervals                        ##
## --------------------------------------------##

## A canned function to do it all at once
t.test(x_sample, mu=5)      ## "Fail to reject the null"

## What about other null hypotheses?
t.test(x_sample, mu = 7.5)  ## "Reject the null"

## Note: t.test also automatically produces a 95% confidence interval.
##       If you want to change the level, can change conf.level option:

t.test(x_sample, mu=5, conf.level = 0.99)

## if you want just the confidence interval, you can "extract" it from the t.test object
t.test(x_sample, mu=5, conf.level = 0.99)$conf.int
