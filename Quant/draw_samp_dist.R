## This function is going to give us something to use over and again


draw_samp_dist = function(N_obs = 100, a_bunch=5000, pop_dist = "Normal"){

  xbar_stor <- rep(NA, a_bunch)  ## just as above

  if(pop_dist=="Normal"){
    for(i in 1:a_bunch){
      this_sample <- rnorm(N_obs, mean=5, sd=1)
      this_xbar   <- mean(this_sample)
      xbar_stor[i] <- this_xbar
    }
    the_population <<- rnorm(10000, mean=5, sd=1)
    hist(the_population)
  }
  
  if(pop_dist == "Uniform"){
    for(i in 1:a_bunch){
      this_sample <- runif(N_obs, min=0, max=10)
      this_xbar   <- mean(this_sample)
      xbar_stor[i] <- this_xbar
    }
    the_population <<- runif(10000, min=0, max=10)
    hist(the_population)
  }
  
  if(pop_dist == "Binomial"){
    for(i in 1:a_bunch){
      this_sample <- rbinom(N_obs, size=10, prob =0.1)
      this_xbar   <- mean(this_sample)
      xbar_stor[i] <- this_xbar
    }
    the_population <<- rbinom(10000, size=10, prob =0.1)
    hist(the_population)
  }
  return(xbar_stor)
}

draw_sample_somehow <- function(N_obs = 100){
  out <- rnorm(N_obs, mean=5, sd=1)
}