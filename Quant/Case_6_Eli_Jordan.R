#-----------------------------
# Author: Eli Jordan
# Case 6 - Sampling Home Prices
#------------------------------


download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")


area <- ames$Gr.Liv.Area
price <- ames$SalePrice


# Exercise 1:
# Both of these histograms are very skewed left. Not a lot of room on the left side of the mean
# Not a very symmetric distribution as a result (no negative prices or areas). 
summary(area)
hist(area)
# Area histogram looks skewed left, the mean is ~1499 with a max 5642

summary(price)
hist(price)
# also skewed heavily left

set.seed(202)

# ------------------------- Part I ---------------------------

samp1 <- sample(area,50)
mean(samp1) # 1478.18, this obviously could change
# Exercise 2: I would love to give a numeric answer for how close it is to the pop mean,
# however there is no guarantee it will be within the 5% like this is. 
# However it is close, and I can say it will always be close.

sample_means50 <- rep(NA,50000)
for (i in 1:50000) {
  sample_means50[i] <- mean(sample(area,50))
}

hist(sample_means50,breaks=30)
# Exercise 3: Nothing much really changes between 5_000 and 50_000. It got a little bit smoother
# but the overall shape was the same.

# ------------------------- Part II ---------------------------

sample_means10 <- rep(NA, 5000)
sample_means100 <- rep(NA, 5000)

for(i in 1:5000){
  samp <- sample(area, 10)
  sample_means10[i] <- mean(samp)
  samp <- sample(area, 100)
  sample_means100[i] <- mean(samp)
}

par(mfrow = c(3, 1))

xlimits <- range(sample_means10)

hist(sample_means10, breaks = 20, xlim = xlimits)
hist(sample_means50, breaks = 20, xlim = xlimits)
hist(sample_means100, breaks = 20, xlim = xlimits)

# ------------------------- Part III ---------------------------

# do 1 sample
price_samp <- sample(price, 50)
mean(price_samp) - mean(price) # difference between sample mean and pop mean

# do a bunch of samples
samples_price_50 <- rep(NA, 5000)
for (i in 1:5000) {
  samples_price_50[i] <- mean(sample(price, 50))
}


# do more samples with different sample sizes
samples_price_10 <- rep(NA, 5000)
samples_price_100 <- rep(NA, 5000)
for (i in 1:5000) {
  samples_price_10[i] <- mean(sample(price, 10))
  samples_price_100[i] <- mean(sample(price, 100))
}

#plot all the new sample size 
par(mfrow = c(3, 1))

xlimits <- range(samples_price_50)


hist(samples_price_10, breaks = 20, xlim = xlimits)
hist(samples_price_50, breaks = 20, xlim = xlimits)
hist(samples_price_100, breaks = 20, xlim = xlimits)