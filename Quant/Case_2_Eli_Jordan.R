# -----------------
# Case 2: Keno Simulations
# Author: Eli Jordan
# MSBC 5031 - Quant Methods
# ----------------- 

#create vector of possible numbers
dealer_draw <- 1:80

# --------------- Part I ------------------------
# Play Keno 
numbers_on_the_board <- sample(dealer_draw, 20, replace = FALSE) # 2. A (pseudo)random game of Keno
# 3. Generic comment about the lines below
set.seed(420) # 5. Set the seed with your favorite number
# bad example numbers because of pseudo-random generator
my_numbers <- c(1, 2, 3, 4, 5, 6, 7, 8) # 6. Your 8 chosen numbers
numbers_on_the_board_game <- sample(dealer_draw, 20, replace = FALSE) # 7. An instance of the Keno game
sum(my_numbers %in% numbers_on_the_board_game) # 8. Compute the number of hits

#------------ Part II -----------------

# play_keno
# parameters - vector of numbers
# return - how many numbers hit

play_keno <- function(my_numbers){
  numbers_on_the_board <- sample(dealer_draw, 20, replace = FALSE)
  how_many_came_up <- sum(my_numbers %in% numbers_on_the_board)
  return(how_many_came_up)
}
# keno_payout
# calculates the payout from a keno game
# parameters - number of hits
# returns - payout based on the table
keno_payout <- function(how_many){
  samp_space   <- 0:8                               ## the nine outcomes that can come up
  payouts      <- c(0,0,0,0, 2, 10, 50, 400, 15000) ## the payouts for each of these 9 outcomes
  payout_table <- rbind(samp_space,payouts)         

  which_col <- payout_table["samp_space",] == how_many
  my_payout <- payout_table["payouts", which_col]
  return(my_payout)
}

samp_space <- 0:8 ## the nine outcomes that can come up
payouts <- c(0,0,0,0, 2, 10, 50, 400, 15000) ## the payouts for each of these 9 outcomes
payout_table <- rbind(samp_space,payouts) 

payout_table                 ## 1. This creates and displays a 2x9 matrix. The first row is the number of matches (0-8) and the second row is the corresponding payout.

payout_table["samp_space",]  ## 2. This selects and displays only the "samp_space" ~row~ from the payout_table matrix.

how_many <- 5  ## test case
payout_table["samp_space",] == how_many  ## 3. This performs a logical comparison, returning a vector of TRUE/FALSE values indicating which element in the "samp_space" row is equal to 5.
which_col <- payout_table["samp_space",] == how_many

payout_table["payouts", ]  ## 4. This selects and displays the entire "payouts" row.

payout_table["payouts", which_col] ## 5. This uses the logical vector `which_col` to select the element from the "payouts" row that corresponds to the `TRUE` value, effectively looking up the payout for 5 matches.

# new numbers
playa_numz = c(45, 32, 27, 18, 24, 5, 73, 12)
simulation_length = 10000
rounds_list <- rep(NA, simulation_length)

for(i in 1:simulation_length){
  rounds_list[i] <- keno_payout(play_keno(playa_numz))
}

# While a histogram is used for continuous data, your payout data is discrete
# (it can only take specific values: 0, 2, 10, etc.). A bar plot is a better
# tool for visualizing the frequency of each specific outcome.

# 1. Tally the frequency of each payout amount
payout_summary <- table(rounds_list)

# 2. Create a bar plot of the results
barplot(payout_summary,
        main = "Payouts from 10,000 Keno Plays",
        xlab = "Payout Amount ($)",
        ylab = "Number of Times Won (Frequency)",
        col = "skyblue")
