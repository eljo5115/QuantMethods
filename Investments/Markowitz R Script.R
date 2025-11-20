## ------------------------- ##
## Project 3 Script Template ##
## ------------------------- ##
set.seed(3)
library(MASS)

## ------------------------------------- ##
## Starting parameters and background    ##
##                                       ##
## Run the code from the first 100 lines ##
## by highlighing and pressing           ##
## Control or Command and Enter.         ##
## ------------------------------------- ##

gen_dat = function(N){ ## see comment below about the purpose of this function.  This depends on running line 5 above to use the mvrnorm() command.
    exp_rets = c(0.06, 0.04, 0.025)
    std_devs = c(0.20, 0.10, 0.07)
    cor_mat  = matrix(c(1    , 0.389 , -0.05 ,
                    0.389, 1     , -0.201,
                   -0.05 , -0.201, 1), byrow=TRUE, nrow=3, ncol=3)
    std_mat = std_devs %*% t(std_devs)  ## computes the cross products as needed for covariances
    cov_mat = cor_mat*std_mat

    dat = as.data.frame(mvrnorm(N, exp_rets, cov_mat))
    names(dat) = c("Rs", "Rb", "Rc")
    return(dat)
}

## the gen_dat() function, defined above, creates a data set of 
## N holding period returns with the covariances and expected
## returns given in the 3x3 example case from the Excel template

df = gen_dat(100)   ## Testing it out, 100 holding periods

head(df) ## checking it works... run this line
## If you did it right, this should print the following:
##             Rs          Rb          Rc
# 1 -0.113494279 -0.08695656 -0.03631117
# 2 -0.007820141  0.06589192  0.03251709
# 3  0.130594517 -0.01411745  0.12295463 ...

## ------------------------------------------- ##
## The following code implements "solver" in R ##
## ------------------------------------------- ##

## ------------------------------------------------- ##
## The following functions implement a "solver" in R ## 
## for the 3x3 case done in Excel.                   ##
##                                                   ##
## The function port_three_sr() calculates the       ## 
## Sharpe ratio (ER_port/SD_port) given expected     ##
## returns, standard deviations and a correlation    ##
## matrix.                                           ##
##                                                   ##
## Using this function, find_max_sharpe() finds the  ##
## weights w_s, w_b, and w_c that maximize the       ##
## Sharpe ratio, given a sample size of simulated    ##
## data from the population you studied in Excel.    ##
##                                                   ##
## You will use find_max_sharpe() to show how much   ##
## the conclusions from the Excel solver depend on   ##
## the amount of historical data.                    ##
## ------------------------------------------------- ##

port_three_sr=function(w_s, w_b, xbar, stdev, correl){ 
  w_vec = c(w_s, w_b, 1-w_s-w_b)  ## stores the portfolio weights in a vector
  std_mat = stdev %*% t(stdev)    ## computes a matrix of stddev_i*stdev_j, for covariance = correl_ij*stdev_i*stdev_j
  cov_mat = correl*std_mat        ## calculates covariances
  w_mat   = w_vec %*% t(w_vec)    ## produces the border-multiplied part of the border multiplied covariance matrix.
  bm_cov_mat = w_mat*cov_mat      ## creates the border multiplied covariance matrix
  
  port_sd = sqrt(sum(bm_cov_mat)) ## square root of the portfolio variance
  port_ER = sum(w_vec*xbar)       ## this is R's version of "sumproduct"
  Rf = 0.01                       ## specify the riskfree rate
  return((port_ER-Rf)/port_sd)    ## returns the Sharpe ratio
}

find_max_sharpe = function(N){
  df = gen_dat(N)                 ## generates N holding periods of data
  xbarr = colMeans(df)            ## sample average
  stdevv = unlist(lapply(df, sd)) ## sample standard deviation
  correll = cor(df)               ## sample correlations
  fun_sr = function(w){           ## this function just formats the function for the optim routine in R (don't worry too much about this)
    w_s = w[1]   ## input is a vector (per optim), but first element is weight on stocks
    w_b = w[2]   ## second element is weight on bonds
    return(-port_three_sr(w_s, w_b, xbarr, stdevv,correll))  ## returns the *negative* of the sharpe ratio (optim minimizes, so minimizing -f(x) gives the max of f(x))
  }
  wtz = optim(c(0,0), fun_sr)$par ## c(0,0) are initial conditions for the weights, the $par just returns the solved weights
  
  w_df = data.frame(w_s = wtz[1], w_b = wtz[2], w_c = 1-wtz[1]-wtz[2])  ## formatting everything as "data"
  return(w_df)  ## returns the vector of solved portfolio weights
}

find_max_sharpe(1000000)   ## Testing out the function
## note that find_max_sharpe(1000000) gives approximately the same
## answer as the Excel solver.  This is a large sample giving a 
## close approximation to reality.


## -------- ##
## Part 3.1 ##
## -------- ##



## --------- ##
## Part 3.2  ##
## --------- ##
input_mu = c(0.06, 0.04, 0.025)
input_sd = c(0.20, 0.10, 0.07)
input_cor  = matrix(c(1    , 0.389 , -0.05 ,
                    0.389, 1     , -0.201,
                    -0.05 , -0.201, 1), byrow=TRUE, nrow=3, ncol=3)

w_s = 0.09552
w_b = 0.3857
w_c = 0.5190

port_three_sr(w_s, w_b, input_mu, input_sd, input_cor)

# matches the excel solver using Rf = 1%

## ----------##
## Part 3.3. ##
## --------- ##
set.seed(1)
find_max_sharpe(100)
find_max_sharpe(100)
find_max_sharpe(100)

find_max_sharpe(1000)
find_max_sharpe(1000)
find_max_sharpe(1000)

find_max_sharpe(10000)
find_max_sharpe(10000)
find_max_sharpe(10000)

## ------------------------------ ##
## Comment here: What do the previous calculations (Part 3.3) tell you?    
## 
## find_max_sharpe(n) gives us the sharpe ratio from n iterations by creating a sample distribution from
## N samples. It tells us an experimental max sharpe ratio.
## Theoretically,  more samples would be closer to the population mean. 
##
## ------------------------------ ##

## ------------------------------- ##
## Part 3.4.                       ##
## Working code to get you started ##
## ------------------------------- ##
birthdate =  11112000   ## input your birthdate here in MMDDYYYY format, e.g., 09171982
set.seed(birthdate)
B = 1000   ## number of different samples
stor_df = NULL

sharpe_months <- function(months){
  for(b in 1:B){   ## When you run this, it takes a few seconds
    this_weights = find_max_sharpe(months)
    stor_df = rbind(stor_df, this_weights)
  }
  par(mfrow=c(1, 3))
  hist(stor_df$w_s, main="Sampling Dist of Weight on Stock",xlim=c(0,1),breaks=20)
  hist(stor_df$w_b, main="Sampling Dist of Weight on Bonds",xlim=c(0,1),breaks=20)
  hist(stor_df$w_c, main="Sampling Dist of Weight on Commodities",xlim=c(0,1),breaks=20)
  
  summary(stor_df)
}

## With monthly data, 120 holding periods is 10 years of information.
## Copy the working code from Part 3.4 above below to show the difference
## in confidence in using 5 years of monthly data and 30 years of monthly data
## and compare to the answers you obtained from the working code I provided.
sharpe_months(60)
sharpe_months(120)
sharpe_months(360)

# 60 month test:
# Mean
# w_s: 0.11196, w_b: 0.3940, w_c: 0.4941
# 120 month test:
# Mean
# w_s: 0.9647, w_b: 0.3949, w_c: 0.5086
# 360 month test:
# Mean
# w_s: 0.0982, w_b: 0.3845, w_c: 0.5174
# 
# There's two takeaways from these differences: the first being more data assumes more accuracy, the second is newer data 
# shows current trends. If we throw in data from a long period, it also includes weird 'outliers' like the 2008 GFC and 2020 Covid
# but using more current data (2020-2025) we can see trends that might be more relevant to current market conditions.
# However with synthetic data, like we're producing here for find_max_sharpe, the longer
# periods would be more representative of a longer holding period (5,10,30 year) so
# would be used to construct a passive portfolio over each respective period. 
# Also, depending on the data, the results can be altered to show better results than reality.