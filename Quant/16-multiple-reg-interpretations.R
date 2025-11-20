## ------------------------------------ ##
## Multiple Regression Interpretations  ##
## Author: Tony Cookson                 ##
## ------------------------------------ ##
if(!require("lfe")){
  install.packages("lfe")
}
if(!require("ggplot2")){
  install.packages("ggplot2")
}
library(lfe)      ## 
library(ggplot2)  ## install.packages("ggplot2")

## Major League Baseball Example ##
setwd("C:/Users/elijo/QuantMethods/Quant/data")
mlb = read.csv("mlb11.csv", header=TRUE)

## Holding Constant Interpretation ##
hitz = lm(wins~hits, data=mlb)
summary(hitz)

hitzrunz = lm(wins~hits+runs, data=mlb)
summary(hitzrunz)


source("binscatter_function.R")

## binscatter for interpretations ##
binscatter("wins~hits", key_var = "hits", bins=5, data=mlb)  

## What does multiple regression do?  Residual interpretation is really nice
binscatter("wins~hits+runs", key_var = "hits", bins=5, data=mlb, partial=TRUE) ## plots residual wins versus residual hits
binscatter("wins~hits+runs", key_var = "runs", bins=5, data=mlb, partial=TRUE) ## plots residual wins versus residual runs
