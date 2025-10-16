## ---------------------------------------- ##
## An illustration of where this unit fits  ##
## into statistics                          ##
## ---------------------------------------- ##

## Get data - ##
set.seed(11112000)    ## input your birthday... e.g., for me Sept 17, 1982 ==> set.seed(09171982)
x_sample <- runif(100, min=5, max=15)

## Given data, compute statistics ##
mean(x_sample)
sd(x_sample)/sqrt(100)
# 2 std errors from sample mean, 95% probability of containing population mean
mean(x_sample) + 2*sd(x_sample)/sqrt(100)*c(-1,1) # 95% confidence interval
