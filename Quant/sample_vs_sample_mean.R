## Illustration of Case 6
## Sampling distribution
## Just for the area

load("ames.RData") ## load data after having set the working directory
area <- ames$Gr.Liv.Area ## define the area variable

## this was like the population
hist(area, breaks=50) ## breaks = 50 is there just to show us the granularity of the distribution

## this was like "the sample we see in reality"
samp1 <- sample(area, 50)
hist(samp1) ## it's a fuzzy copy of the population
            ## not normal, not closer to normal as the sample size gets larger

samp2 <- sample(area, 100)
hist(samp2) ## it's a fuzzy copy of the population
## not normal, not closer to normal as the sample size gets larger

samp3 <- sample(area, 200)
hist(samp3) ## it's a fuzzy copy of the population
## not normal, not closer to normal as the sample size gets larger


## Let's do a different animation from last week
## plot hist(samp) instead of hist(sample_means50)

for(i in 1:5000){
  samp <- sample(area, 200)
  sample_means50[i] <- mean(samp)
  hist(samp)
  Sys.sleep(0.05)
}

hist(sample_means50)