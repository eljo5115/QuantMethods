#--------------
# Author: Eli Jordan
# Roulette sim to find EV for various strategies
#--------------

# Function to simulate a single American roulette spin
spin_american_roulette <- function() {
  pockets <- c(0, "00", 1:36)
  sample(pockets, 1)
}
# Function to determine the color of an American roulette pocket
get_american_roulette_color <- function(pocket) {
  if (pocket == 0 || pocket == "00") {
    return("green")
  } else if (pocket %in% c(1, 3, 5, 7, 9, 12, 14, 16, 18, 19, 21, 23, 25, 27, 30, 32, 34, 36)) {
    return("red")
  } else {
    return("black")
  }
}

# Function to simulate a single American roulette spin and return pocket and color
spin_american_roulette_with_color <- function() {
  pockets <- c(0, "00", 1:36)
  result_pocket <- sample(pockets, 1)
  result_color <- get_american_roulette_color(result_pocket)
  
  return(list(pocket = result_pocket, color = result_color))
}

spin_american_roulette_with_color()

# Function to simulate a single American roulette spin with a betting strategy
simulate_strategy <- function(strategy_func, initial_bankroll, num_spins) {
  bankroll <- initial_bankroll
  
  for (i in 1:num_spins) {
    # Place bet according to the strategy
    bet_result <- strategy_func(bankroll)
    bet_amount <- bet_result$bet_amount
    bet_type <- bet_result$bet_type
    
    if (bankroll < bet_amount) {
      message("Bankroll too low to place bet. Strategy simulation ended.")
      break
    }
    
    bankroll <- bankroll - bet_amount
    
    # Spin the wheel
    spin_outcome <- spin_american_roulette_with_color()
    
    # Determine win/loss
    if (bet_type == "red" && spin_outcome$color == "red") {
      bankroll <- bankroll + bet_amount * 2 # Payout for red/black is 1:1
    } else if (bet_type == "black" && spin_outcome$color == "black") {
      bankroll <- bankroll + bet_amount * 2
    } else if (bet_type == "green" && spin_outcome$color == "green") {
      bankroll <- bankroll + bet_amount * 36 # Payout for single number (green) is 35:1 + original bet
    } else if (bet_type == "number" && spin_outcome$pocket == bet_result$bet_value) {
      bankroll <- bankroll + bet_amount * 36
    }
    # Add more conditions for other bet types as needed
    
    # You might want to record bankroll history here
  }
  
  return(bankroll)
}

# Example strategy: Always bet $1 on Red
strategy_always_red <- function(current_bankroll) {
  return(list(bet_amount = 1, bet_type = "red"))
}

strategy_always_red()
# Example strategy: Always bet $1 on a specific number (e.g., 7)
strategy_always_number_7 <- function(current_bankroll) {
  return(list(bet_amount = 1, bet_type = "number", bet_value = 7))
}

# Monte Carlo Simulation
run_monte_carlo_simulation <- function(strategy_func, initial_bankroll, num_spins_per_run, num_runs, simulator_func = simulate_strategy) {
  final_bankrolls <- numeric(num_runs)
  
  for (i in 1:num_runs) {
    final_bankrolls[i] <- simulator_func(strategy_func, initial_bankroll, num_spins_per_run)
  }
  
  return(final_bankrolls)
}

# Parameters for the Monte Carlo simulation
initial_bankroll <- 1000
num_spins_per_run <- 100
num_runs <- 1000

# Run Monte Carlo for strategy_always_red
mc_results_red <- run_monte_carlo_simulation(strategy_always_red, initial_bankroll, num_spins_per_run, num_runs)
print(paste("Average final bankroll for 'always red' strategy:", mean(mc_results_red)))
print(paste("Probability of profit for 'always red' strategy:", sum(mc_results_red > initial_bankroll) / num_runs))

hist(mc_results_red, breaks = 50, xlab = "Final Bankroll", ylab = "Frequency",)

# Run Monte Carlo for strategy_always_number_7
mc_results_number_7 <- run_monte_carlo_simulation(strategy_always_number_7, initial_bankroll, num_spins_per_run, num_runs)
print(paste("Average final bankroll for 'always number 7' strategy:", mean(mc_results_number_7)))
print(paste("Probability of profit for 'always number 7' strategy:", sum(mc_results_number_7 > initial_bankroll) / num_runs))

# Strategy: Bet $1 on Red and $1 on each Red number (18 red numbers)
strategy_red_and_red_numbers <- function(current_bankroll) {
  # Total bet will be $1 for Red + $1 for each of the 18 red numbers = $19
  return(list(bet_amount = 19, bet_type = "red_and_red_numbers"))
}

# Modify simulate_strategy to handle the new bet type
simulate_strategy_mixed <- function(strategy_func, initial_bankroll, num_spins) {
  bankroll <- initial_bankroll
  
  for (i in 1:num_spins) {
    bet_result <- strategy_func(bankroll)
    # Ensure bet_result is a list with bet_amount and bet_type
    bet_amount <- bet_result$bet_amount
    bet_type <- bet_result$bet_type
    
    if (bankroll < bet_amount) {
      message("Bankroll too low to place bet. Strategy simulation ended.")
      break
    }
    
    bankroll_after_bet <- bankroll - bet_amount
    
    spin_outcome <- spin_american_roulette_with_color()
    
    winnings <- 0
    
    if (bet_type == "red") {
      if (spin_outcome$color == "red") {
        winnings <- 2 # Payout for red/black is 1:1 (bet_amount * 2)
      }
    } else if (bet_type == "black") {
      if (spin_outcome$color == "black") {
        winnings <- 2
      }
    } else if (bet_type == "green") {
      if (spin_outcome$color == "green") {
        winnings <- 36 # Payout for single number (green) is 35:1 + original bet
      }
    } else if (bet_type == "number") {
      if (spin_outcome$pocket == bet_result$bet_value) {
        winnings <- 36
      }
    } else if (bet_type == "red_and_red_numbers") {
      # This strategy bets $1 on Red and $1 on each of the 18 red numbers. Total bet is $19.
      # If a red number hits, the $1 "Red" bet pays 1:1 (returns $2)
      # and the $1 bet on the specific number pays 35:1 (returns $36).
      # Total return is $38.
      if (spin_outcome$color == "red") {
        winnings <- 38
      }
    }
    bankroll <- bankroll_after_bet + winnings
  }
  
  return(bankroll)
}

# Run Monte Carlo for strategy_red_and_red_numbers using the mixed simulator
mc_results_red_and_red_numbers <- run_monte_carlo_simulation(strategy_red_and_red_numbers, initial_bankroll, num_spins_per_run, num_runs)
print(paste("Average final bankroll for 'red and red numbers' strategy:", mean(mc_results_red_and_red_numbers)))
print(paste("Probability of profit for 'red and red numbers' strategy:", sum(mc_results_red_and_red_numbers > initial_bankroll) / num_runs))

