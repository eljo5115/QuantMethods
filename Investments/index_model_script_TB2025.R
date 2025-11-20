## --------------------------------- ##
## Index Model Script                ##
## --------------------------------- ##
setwd("/Users/eli/noticloud/Documents/GitHub/QuantMethods/Investments")
idx = read.csv("Index_model_Data.csv")

## ----------------------------- ##
## Introduction to Index Models  ##
## ----------------------------- ##

## Fit a security characteristic line ##
plot(MSFTret~SPYret, data=idx)
msft_lm = lm(MSFTret~SPYret, data=idx)  ## just a regression
abline(coef(msft_lm))  ## plot the best fit line

summary(msft_lm)  ## provides all of the information we'd want

## alpha and beta are here
coef(msft_lm)

## residual sd is here.  This is sigma(e)
summary(msft_lm)$sigma

## can write a function to give these three bits of information ##
index_model_inputs = function(my_lm){
  my_vec = c(coef(my_lm), summary(my_lm)$sigma)
  dat_out = data.frame(alpha=my_vec[1], beta=my_vec[2],sigma_e = my_vec[3])
  return(dat_out)
}

index_model_inputs(msft_lm)

## --------------------------------------- ##
## What follows is the Treynor-Black       ##
## Algorithm for forming an optimal        ##
## portfolio of active/passive investments ##
## --------------------------------------- ##

## Step 1: Fit the security characteristic lines ##
## Do this for all of the assets you are interested in including

a_lm = lm(Aret~SPYret, data=idx)
mmm_lm = lm(MMMret~SPYret, data=idx)
msft_lm = lm(MSFTret~SPYret, data=idx)
f_lm = lm(Fret~SPYret, data=idx)
aapl_lm = lm(AAPLret~SPYret, data=idx)

datta = rbind(index_model_inputs(a_lm),
              index_model_inputs(mmm_lm),
              index_model_inputs(msft_lm),
              index_model_inputs(f_lm),
              index_model_inputs(aapl_lm))    ## This code just binds all of the index model inputs together in one data frame.

row.names(datta) = c("A", "MMM", "MSFT", "F", "AAPL")

## Step 2: Compute the portfolio weights ##
datta$pw = datta$alpha/(datta$sigma_e^2)
datta$active_weights = datta$pw/sum(datta$pw)

## Step 3: Compute the portfolio characteristics ##
port_alpha = sum(datta$alpha*datta$active_weights)
port_beta = sum(datta$beta*datta$active_weights)
port_sigma_e = sqrt(sum((datta$sigma_e*datta$active_weights)^2))

## Step 4: Decide on active share versus passive share ##
mkt_rp = mean(idx$SPYret,na.rm=TRUE)
mkt_vol = sd(idx$SPYret,na.rm=TRUE)^2

initial_wt = port_alpha/(port_sigma_e^2)/(mkt_rp/mkt_vol)  ## Intuition: How attractive is the active portfolio relative to the market index?

active_wt = initial_wt/(1+(1-port_beta)*initial_wt)   ## Adjust for how exposed the market portfolio is to market risk

## Step 5: Piece everything together
final_port_wts = c(1-active_wt, active_wt*datta$active_weights)
names(final_port_wts) = c("SPY", "A", "MMM", "MSFT", "F", "AAPL")

## Report the final optimal portfolio ##
final_port_wts


## do it for a, mmm and spy
a_lm = lm(Aret~SPYret, data=idx)
mmm_lm = lm(MMMret~SPYret, data=idx)

datta = rbind(index_model_inputs(a_lm),
              index_model_inputs(mmm_lm))    ## This code just binds all of the index model inputs together in one data frame.

row.names(datta) = c("A", "MMM")

## Step 2: Compute the portfolio weights ##
datta$pw = datta$alpha/(datta$sigma_e^2)
datta$active_weights = datta$pw/sum(datta$pw)

## Step 3: Compute the portfolio characteristics ##
port_alpha = sum(datta$alpha*datta$active_weights)
port_beta = sum(datta$beta*datta$active_weights)
port_sigma_e = sqrt(sum((datta$sigma_e*datta$active_weights)^2))

## Step 4: Decide on active share versus passive share ##
mkt_rp = mean(idx$SPYret,na.rm=TRUE)
mkt_vol = sd(idx$SPYret,na.rm=TRUE)^2

initial_wt = port_alpha/(port_sigma_e^2)/(mkt_rp/mkt_vol)  ## Intuition: How attractive is the active portfolio relative to the market index?

active_wt = initial_wt/(1+(1-port_beta)*initial_wt)   ## Adjust for how exposed the market portfolio is to market risk

## Step 5: Piece everything together
final_port_wts = c(1-active_wt, active_wt*datta$active_weights)
names(final_port_wts) = c("SPY", "A", "MMM")

## Report the final optimal portfolio ##
final_port_wts

