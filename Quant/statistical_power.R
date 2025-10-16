## ----------------------- ##
## Computing and Graphing  ##
## Statistical Power       ##
## ----------------------- ##

source("shadenorm.R")  ## Run this line to source the shadenorm() function that we employ here.

## Parameters of the Problem ##

mu0   = 6     ## Null-hypothesized value
alpha = 0.05  ## Significance Level
s.sq  = 16    ## Variance from pilot study
n1    = 64    ## Sample Size
alt =  6.5

se = sqrt(s.sq/n1)

## -------------------------- ##
## What is Statistical Power? ##
## -------------------------- ##

cuts = c(1-alpha/2, alpha/2)
crits = qnorm(cuts, mu0, sqrt(s.sq/n1))    ## Critical Values Based on Null
shadenorm(mu=mu0, sig = se, outside=crits) ## My own function
shadenorm(mu = 6.5, sig = se, lines=TRUE, outside=crits, col="blue")

## ------------------------------ ##
## Write a Function to Compute it ##
## ------------------------------ ##

power = function(theta, mu, var, n, alpha=0.05){
  crit.l = qnorm(alpha/2, mu, sqrt(var/ n))    ## Critical Value Based on Null
  crit.h = qnorm(1-alpha/2, mu, sqrt(var/ n))  ## Critical Value Based on Null
  pr.high = pnorm(crit.h, theta, sd = sqrt(var/ n),lower.tail=FALSE) ## Prob Reject High
  pr.low  = pnorm(crit.l, theta, sd = sqrt(var/ n))                  ## Prob Reject Low
  pow = pr.low+pr.high
  
  pow
  
}

power(thet=6.5, mu=6, var= 16, n = 64)
thet = seq(3,9, by=0.01)

pow = power(thet, mu0, s.sq, n1)
plot(thet, pow, type="l", ylim=c(0,1), main="My Power Plot")

n2 = 256  ## New Larger Sample Size
pow2 = power(thet, mu0, s.sq,n2)
lines(thet, pow2, lty="dashed", col="blue")
abline(h=0.05, lty="dotted")
