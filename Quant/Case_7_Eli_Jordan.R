# --------------
# Author: Eli Jordan
# Case 7: Confidence Intervals
# --------------


download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")
set.seed(1709)   ## use this seed for this lab. don't deviate!


# ---------------------- Part I ------------------------
sample_size <- 60

population <- ames$Gr.Liv.Area
samp <- sample(population, sample_size)

sample_mean <- mean(samp)

# sample size 60
se <- sd(samp) / sqrt(sample_size)
lower <- sample_mean - qt(0.975,sample_size-1) * se
upper <- sample_mean + qt(0.975,sample_size-1) * se
c(lower, upper) # (1528,1794)

# The Central Limit Theorem states that the sampling distribution approaches normal
# for a sufficiently large number of samples. So for a single sample we can make a 
# 'good guess' at an interval with the standard error from a single samplem mean because
# of this property; if the sample is sufficiently

# Exercise 1:

se <- sd(samp) / sqrt(sample_size)
lower <- sample_mean - qnorm(0.05, lower.tail = FALSE) * se
upper <- sample_mean + qnorm(0.05, lower.tail= FALSE) * se
c(lower,upper) # 1551,1770
# It is slightly different (~20 different each side). However we can't use it because
# it makes assumptions about the population distribution that we cannot assume. The 
# t distribution is not normally distributed, and results in a more conservative 
# interval.

# ---------------------- Part II ------------------------

samp_mean <- rep(NA, 50)
samp_sd <- rep(NA, 50)
n <- 60

# 60 samples of size 60
for(i in 1:60){
  samp <- sample(population, n) # obtain a sample of size n = 60 from the population
  samp_mean[i] <- mean(samp)    # save sample mean in ith element of samp_mean
  samp_sd[i] <- sd(samp)        # save sample sd in ith element of samp_sd
}

lower_vector <- samp_mean - qt(0.975,59) * samp_sd / sqrt(n) 
upper_vector <- samp_mean + qt(0.975,59) * samp_sd / sqrt(n)

c(lower_vector[1], upper_vector[1])

plot_ci(lower_vector, upper_vector, mean(population))

# I had 2 intervals miss the mark
# 2/60 = 0.0333 = 3.3%
# This is expected for a 95% confidence intervals, as 5% should miss the true mean
# 3.33% off is very expected as the randomness can produce some variance

# running without the seed results in 3 or 4 intervals missing.