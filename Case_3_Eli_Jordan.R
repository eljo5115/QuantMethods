#---------------
# Case 3: The Birthday Proble
# Author: Eli Jordan
# MSBC 5031 - Quant Methods
#---------------

# set seed
set.seed(2)
# days in a year on earth (excluding Feb 29th)
days_in_year <- 1:365
# birthday_matches
# function to give a table of birthday matches
# parameters: group_size - number of people in the group
# output: table of matches on each date (given as days from YTD)
birthday_matches <- function(group_size){
  # samples days for 46 people
  samp <- sample(days_in_year, group_size, replace = TRUE)

  tab_samp <- table(samp) > 1

  return(sum(as.numeric(tab_samp)))
}

# simulate_birthday_matches
# simulates birthday problem
# parameters: sim_size - number of time to run the simulation
#            group_size - size of the group to simulate
# output: table counting the collisions that occured
simulate_birthday_matches <- function(sim_size=10000,group_size=50){
  sims <- rep(NA, sim_size)
  for(i in 1:sim_size){
    sims[i] <- birthday_matches(group_size)
  }
  collisions <- table(sims)

  return(collisions)
}

# now we can simulate any group size for any sim length
prob_46 <- simulate_birthday_matches(10000,46)
hist(prob_46)
#or, we can create a table of probabilites and plot it

# max_group = 50
# group_probs = rep(NA, max_group)
# for(i in 1:max_group){
#   group_probs[i] <- simulate_birthday_matches(10000,i)
# }

# barplot(group_probs,ylim=c(0,1),col="skyblue",ylab="Probability of at least 1 match",xlab="Group Size")


occurences_25 <- simulate_birthday_matches(10000,25)
