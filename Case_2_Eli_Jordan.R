# -----------------
# Case 2: Keno Simulations
# Author: Eli Jordan
# MSBC 5031 - Quant Methods
# ----------------- 

#create vector of possible numbers
dealer_draw = 1:80
set.seed(420)
my_numbers = sample(dealer_draw,8,replace=FALSE)


play_keno <- function(my_numbers){
  numbers_on_the_board = sample(dealer_draw)
  how_many_came_up <- sum(my_numbers %in% numbers_on_the_board)
  return(keno_payout(how_many_came_up))
}

keno_payout <- function(how_many){
  samp_space   <- 0:8                               ## the nine outcomes that can come up
  payouts      <- c(0,0,0,0, 2, 10, 50, 400, 15000) ## the payouts for each of these 9 outcomes
  payout_table <- rbind(samp_space,payouts)         

  which_col <- payout_table["samp_space",] == how_many  ## 
  my_payout <- payout_table["payouts", which_col]
  return(my_payout)
}

win = play_keno(my_numbers)


samp_space <- 0:8 ## the nine outcomes that can come up
payouts <- c(0,0,0,0, 2, 10, 50, 400, 15000) ## the payouts for each of these 9 outcomes
payout_table <- rbind(samp_space,payouts) 

payout_table                 ## 1

payout_table["samp_space",]  ## 2

how_many <- 5  ## test case
payout_table["samp_space",] == how_many  ## 3
which_col <- payout_table["samp_space",] == how_many

payout_table["payouts", ]  ## 4

payout_table["payouts", which_col] ## 5