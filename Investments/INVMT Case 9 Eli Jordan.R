## ----------------
# Author: Eli Jordan
# Case 9: Multi Factor Model
# -----------------


## Initial Data Step

setwd("C:/Users/elijo/QuantMethods/Investments/data")
library(quantmod)
install.packages("stargazer")
library(stargazer)


## 1. Download updated monthly data from Yahoo
getSymbols(c("BFOCX", "CL=F", "JGMAX", "JBLU", "SLB"),
           from = as.Date("2020-10-01"),
           to = as.Date("2025-10-31"),
           periodicity = "monthly")
crude <- na.omit(`CL=F`) 
# Convert to monthly returns for each ticker
monthly_ret <- function(sym) {
  px <- Ad(get(sym))                        # adjusted prices
  mo <- to.monthly(px, indexAt = "lastof", OHLC = FALSE)
  ret <- diff(mo) / lag(mo, 1)             # simple returns
  df <- data.frame(Date = as.Date(index(ret)),
                   ret  = as.numeric(ret))
  df$monthyr <- format(df$Date, "%Y%m")
  return(df)
}

## Fund / stock monthly returns
bfocx_ret <- monthly_ret("BFOCX")
jgmax_ret <- monthly_ret("JGMAX")
jblu_ret  <- monthly_ret("JBLU")
slb_ret   <- monthly_ret("SLB")

## Crude oil (CL=F) monthly returns
crude_ret <- monthly_ret("CL=F")


## 2. Load Fama–French factors from zipped class file
F_raw <- read.csv("F-F_Research_Data_Factors.csv",
                  header = TRUE, stringsAsFactors = FALSE)

# Fix column names 
colnames(F_raw)[1] <- "monthyr"

## Keep monthly observations, last 60+ months
F_raw <- F_raw[grepl("^[0-9]{6}$", F_raw$monthyr), ]
F_raw <- subset(F_raw, monthyr >= "202010")

F_raw$Date    <- as.Date(paste0(F_raw$monthyr, "01"), format = "%Y%m%d")
F_raw$monthyr <- as.character(F_raw$monthyr)

## Clean FF dataframe
F.F <- data.frame(
  monthyr = F_raw$monthyr,
  Rm      = F_raw$Mkt.RF,   # Market excess return (MKT-RF)
  SMB     = F_raw$SMB,      # Size factor
  HML     = F_raw$HML,      # Value factor
  Rf      = F_raw$RF        # Risk-free rate
)

## Merge JGMAX returns with FF factors
dat_jgmax <- merge(jgmax_ret, F.F, by = "monthyr")
dat_jgmax <- tail(dat_jgmax, 60)  # most recent 60 months

## Make sure factor columns are numeric
dat_jgmax$Rm  <- as.numeric(dat_jgmax$Rm)
dat_jgmax$SMB <- as.numeric(dat_jgmax$SMB)
dat_jgmax$HML <- as.numeric(dat_jgmax$HML)
dat_jgmax$Rf  <- as.numeric(dat_jgmax$Rf)

## JGMAX excess return (in percent)
dat_jgmax$ret_pct    <- 100 * dat_jgmax$ret
dat_jgmax$excess_ret <- dat_jgmax$ret_pct - dat_jgmax$Rf

## FF3 regression
jgmax_ff3 <- lm(excess_ret ~ Rm + SMB + HML, data = dat_jgmax)
summary(jgmax_ff3)

## Inputting a Stargazer table for writeup
stargazer(jgmax_ff3,
          type = "text",
          title = "JGMAX – Fama–French 3-Factor Model",
          dep.var.labels = "Excess Monthly Return",
          covariate.labels = c("Market (Rm)", "Size (SMB)", "Value (HML)"),
          digits = 3,
          style = "qje")

## Part 2: BFOCX FF3

## Merge BFOCX returns with FF factors
dat_bfocx <- merge(bfocx_ret, F.F, by = "monthyr")
dat_bfocx <- tail(dat_bfocx, 60)

dat_bfocx$Rm  <- as.numeric(dat_bfocx$Rm)
dat_bfocx$SMB <- as.numeric(dat_bfocx$SMB)
dat_bfocx$HML <- as.numeric(dat_bfocx$HML)
dat_bfocx$Rf  <- as.numeric(dat_bfocx$Rf)

dat_bfocx$ret_pct    <- 100 * dat_bfocx$ret
dat_bfocx$excess_ret <- dat_bfocx$ret_pct - dat_bfocx$Rf

bfocx_ff3 <- lm(excess_ret ~ Rm + SMB + HML, data = dat_bfocx)
summary(bfocx_ff3)

stargazer(bfocx_ff3,
          type = "text",
          title = "BFOCX – Fama–French 3-Factor Model",
          dep.var.labels = "Excess Monthly Return",
          covariate.labels = c("Market (Rm)", "Size (SMB)", "Value (HML)"),
          digits = 3,
          style = "qje")

## Part 3: Oil factor + FF3 (SLB, JBLU)

## 5.1 Merge crude oil returns with FF factors
crude_ret$monthyr <- format(crude_ret$Date, "%Y%m")

dat_crude <- merge(crude_ret, F.F, by = "monthyr")
crude_ret$monthyr <- format(crude_ret$Date, "%Y%m")

dat_crude <- merge(crude_ret, F.F, by = "monthyr")

## making columns numeric
dat_crude$ret <- as.numeric(dat_crude$ret)
dat_crude$Rf  <- as.numeric(dat_crude$Rf)

dat_crude$crude_excess <- 100 * dat_crude$ret - dat_crude$Rf


## Crude oil excess return (percent)
dat_crude$crude_excess <- 100 * dat_crude$ret - dat_crude$Rf

## Keep only columns we need for merging with stocks
oil_factors <- dat_crude[, c("monthyr", "Rm", "SMB", "HML", "Rf", "crude_excess")]

dat_crude$Rm  <- as.numeric(dat_crude$Rm)
dat_crude$SMB <- as.numeric(dat_crude$SMB)
dat_crude$HML <- as.numeric(dat_crude$HML)
dat_crude$Rf  <- as.numeric(dat_crude$Rf)
dat_crude$ret <- as.numeric(dat_crude$ret)

dat_crude$crude_excess <- 100 * dat_crude$ret - dat_crude$Rf

oil_factors <- dat_crude[, c("monthyr", "Rm", "SMB", "HML", "Rf", "crude_excess")]

## SLB data + factors + oil
dat_slb <- merge(slb_ret, oil_factors, by = "monthyr")
dat_slb <- tail(dat_slb, 60)

dat_slb$Rm           <- as.numeric(dat_slb$Rm)
dat_slb$SMB          <- as.numeric(dat_slb$SMB)
dat_slb$HML          <- as.numeric(dat_slb$HML)
dat_slb$Rf           <- as.numeric(dat_slb$Rf)
dat_slb$crude_excess <- as.numeric(dat_slb$crude_excess)

dat_slb$ret_pct      <- 100 * dat_slb$ret
dat_slb$excess_ret   <- dat_slb$ret_pct - dat_slb$Rf

slb_ff4 <- lm(excess_ret ~ Rm + SMB + HML + crude_excess, data = dat_slb)
summary(slb_ff4)

## JBLU data + factors + oil
dat_jblu <- merge(jblu_ret, oil_factors, by = "monthyr")
dat_jblu <- tail(dat_jblu, 60)

dat_jblu$Rm           <- as.numeric(dat_jblu$Rm)
dat_jblu$SMB          <- as.numeric(dat_jblu$SMB)
dat_jblu$HML          <- as.numeric(dat_jblu$HML)
dat_jblu$Rf           <- as.numeric(dat_jblu$Rf)
dat_jblu$crude_excess <- as.numeric(dat_jblu$crude_excess)

dat_jblu$ret_pct      <- 100 * dat_jblu$ret
dat_jblu$excess_ret   <- dat_jblu$ret_pct - dat_jblu$Rf

jblu_ff4 <- lm(excess_ret ~ Rm + SMB + HML + crude_excess, data = dat_jblu)
summary(jblu_ff4)

stargazer(slb_ff4, jblu_ff4,
          type  = "text",
          title = "Oil-Sensitive Stocks – 4-Factor Model (FF3 + Oil)",
          dep.var.labels   = "Excess Monthly Return",
          covariate.labels = c("Market (Rm)", "Size (SMB)", "Value (HML)", "Oil (Crude Excess)"),
          digits = 3,
          style = "qje")


