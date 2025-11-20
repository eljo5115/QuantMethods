## -------------------- ##
## SBUX risks in class  ##
## Author: Tony Cookson ##
## -------------------- ##
setwd("/Users/eli/noticloud/Documents/GitHub/QuantMethods/Quant/data")
load("SBUX.RData")  ## see week13_fin_dataprocess.R on how to construct the data for this example.

## Implementing multiple regressions ##
mylm <- lm(ret_sbux~ret_milk, data=dat)
summary(mylm)

mylm <- lm(I(100*ret_sbux)~I(100*ret_milk), data=dat)
summary(mylm)

mylm2 <- lm(ret_sbux~ret_sp + ret_milk, data=dat)
summary(mylm2)

mylm3 <- lm(ret_sbux~ret_sp +ret_coff+ ret_milk+scale(coffee_near_me)+scale(SBUX_search), data=dat)
summary(mylm3)  ## getting a little complicated 5 predictors for 59 observations.
                ## rule of thumb: need at least 10 observations per predictor.

source("binscatter_function.R")
binscatter("ret_sbux~ret_milk", key_var = "ret_milk", 
           data=dat, bins = 10)  ## raw relationship

binscatter("ret_sbux~ret_sp+ret_milk", key_var = "ret_milk", 
           partial=TRUE, data=dat, bins = 10)  ## add s&p500 returns


binscatter("ret_sbux~ret_sp+ret_milk", key_var = "ret_sp", 
           partial=TRUE, data=dat, bins = 10)  ## add s&p500 returns


binscatter("ret_sbux~ret_sp + ret_milk + ret_coff + scale(coffee_near_me) + SBUX_search",
           key_var = "ret_milk", 
           partial=TRUE, data=dat, bins = 10)
