
## An example of getting some index model inputs ##

setwd("/Users/eli/noticloud/Documents/GitHub/QuantMethods/Investments")
dat<- read.csv("MarkoModelData_InClass.csv")


aapl_lm = lm(AAPLret~SPYret, data=dat)

summary(aapl_lm)

coef(aapl_lm)
summary(aapl_lm)$sigma

inx_model_inputs = function(mylm){
 out =  c(coef(mylm), summary(mylm)$sigma)
 return(out)
}

inx_model_inputs(aapl_lm)

## Illustrate this with a two asset case ##
msft_lm <- lm(MSFTret~SPYret, data=dat)


tab <- rbind(inx_model_inputs(aapl_lm),
inx_model_inputs(msft_lm))

colnames(tab) <- c("alpha", "beta", "sigma")
sd(dat$SPYret, na.rm=TRUE)
mean(dat$SPYret, na.rm=TRUE)
tab

## Full covariance matrix
cov(dat[,c("AAPLret", "MSFTret", "SPYret")], use="complete.obs")
mean(dat$AAPLret, na.rm=TRUE)
mean(dat$MSFTret, na.rm=TRUE)
