#---------------
# Case 3: The Birthday Proble
# Author: Eli Jordan
# MSBC 5031 - Quant Methods
#---------------

# set seed
set.seed(2)

#----------------------------Functions---------------------------------------

# birthday_matches
# function to give a table of birthday matches
# parameters: group_size - number of people in the group
#             days - number of days in the year (excluding Feb 29th)
# output: table of matches on each date (given as days from YTD)
birthday_matches <- function(group_size,days=365){
        # days in a year on earth (excluding Feb 29th)
        days_in_year <- 1:days
        # samples days for a group of people
        samp <- sample(days_in_year, group_size, replace = TRUE)

        # Count how many dates have more than one person (a match)
        return(sum(table(samp) > 1))
}

# simulate_birthday_matches
# simulates birthday problem
# parameters: sim_size - number of time to run the simulation
#            group_size - size of the group to simulate
#            days - number of days in the year (excluding Feb 29th)
# output: table counting the collisions that occurred
simulate_birthday_matches <- function(sim_size=10000,group_size=50,days=365){
        # create 'empty' vector to store results
        sims <- rep(NA, sim_size)
        # run sim for sim_size times
        for(i in 1:sim_size){
                sims[i] <- birthday_matches(group_size,days)
        }
        # convert results into a table
        collisions <- table(sims)
        # return the table
        return(collisions)
}

#---------------------------------Implementation------------------------------------

# now we can simulate any group size for any sim length and group size
occurences_46 <- simulate_birthday_matches(10000,46)
# barplot is functionally a histogram when fed a table
barplot(prob_46,
        main = "Birthday Matches for a Group of 46",
        xlab = "Number of Matches",
        ylab = "Frequency (out of 10000 simulations)",
        col = "lightblue")

#
occurences_20 <- simulate_birthday_matches(10000,20)
barplot(occurences_20,
        main = "Birthday Matches for a Group of 20",
        xlab = "Number of Matches",
        ylab = "Frequency (out of 10000 simulations)",
        col = "lightgreen")


#-------------------------------------Variations-----------------------------------------
# Now if we want to see what birthdays on Venus, or Mars or whatever looks like
venus_birthdays_25 <- simulate_birthday_matches(10000,25,243)
barplot(venus_birthdays_25,
        main = "Venus Birthday Matches for a Group of 25",
        xlab = "Number of Matches",
        ylab = "Frequency (out of 10000 simulations)",
        col = "lightblue")

# From the plot, we can see that the fewer days in the year, the more collisions occur

# Or for Mars
mars_birthdays_25 <- simulate_birthday_matches(10000,25,687)
barplot(mars_birthdays_25,
        main="Mars Birthday Matches for a Group of 25",
        xlab="Number of Matches",
        ylab="Frequency (out of 10000 simulations)",
        col="lightgreen")

# What if we knew everyone had a birthday in the same month?
collisions_in_a_month <- simulate_birthday_matches(10000,35,31)
barplot(collisions_in_a_month,
        main="Collisions in a Month"
        )
# The plot reveals that there are 0 occurences of 0 collisions
# This demonstrates the pigeonhole principle
# trying to fit n pigeons into m holes, if n>m there must be at least 1 hole with
# more than 1 pigeon in it. 
# Pretty obvious, but important math