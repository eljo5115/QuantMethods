## ------------------------- ##
## Investment factors case   ##
## ------------------------- ##

## Read in and clean data
setwd("~/Downloads/multifactor data for case")
jgmax = read.csv("JGMAX.csv", header=TRUE)
bfocx = read.csv("BFOCX.csv", header=TRUE)
ff    = read.csv("F-F.csv", header=TRUE)
crude = read.csv("crude.csv", header=TRUE)
jblu  = read.csv("JBLU.csv", header=TRUE)
slb   = read.csv("SLB.csv", header=TRUE)

clean_dat = function(dat){
  Date = as.Date(dat[, 1], format="%m/%d/%y")
  ret_name = names(dat)[length(dat)]
  trim_dat = dat[,names(dat)[c(length(dat)-1, length(dat))]]
  out_dat = cbind(Date, trim_dat)
  out_dat$Volume = NULL
  out_dat$yearmonth = format(out_dat$Date, "%Y%m")
  out_names = c("yearmonth", ret_name)
  out_dat = out_dat[,out_names]
  return(out_dat)
}

jgmax = clean_dat(jgmax)
bfocx = clean_dat(bfocx)
crude = clean_dat(crude)
jblu  = clean_dat(jblu)
slb   = clean_dat(slb)

names(ff)[1]  = c("yearmonth")

dat = merge(jgmax, bfocx)
dat = merge(dat, ff)
dat = merge(dat, jblu)
dat = merge(dat, slb)
dat = merge(dat, crude, all.x = TRUE)

## Let's analyze JGMAX

j_lm = lm(jgmax_ret~Mkt.RF+SMB+HML, data=dat)
summary(j_lm)    ## looks like a small, growth fund... how does Morningstar classify it?

## Let's analyze BFOCX

b_lm = lm(I(100*bfocx_ret)~Mkt.RF+SMB+HML, data=dat)
summary(b_lm)    ## looks like a mid, growth fund... how does Morningstar classify it?

## Let's analyze JBLU and SLB
jblu_lm1 = lm(JBLU_ret~oil_ret, data=dat)
summary(jblu_lm1)

jblu_lm1 = lm(I(100*JBLU_ret)~I(100*oil_ret), data=dat)
summary(jblu_lm1)


jblu_lm2 = lm(I(100*JBLU_ret)~Mkt.RF+SMB+HML+I(100*oil_ret), data=dat)  ## put on the same scale to get percent on percent
summary(jblu_lm2)    ## 

slb_lm1 = lm(I(100*SLB_ret)~I(100*oil_ret), data=dat)
summary(slb_lm1)    ## 

slb_lm2 = lm(I(100*SLB_ret)~Mkt.RF+SMB+HML+I(100*oil_ret), data=dat)
summary(slb_lm2)    ## 

b_lm2 = lm(I(100*bfocx_ret)~Mkt.RF+SMB+HML+I(100*oil_ret), data=dat)
summary(b_lm2) 
