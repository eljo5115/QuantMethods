mu = 75.44
sigma = 8

x_grid = seq(0, 100, by=1)


dx = dnorm(x_grid, mean=mu,sd=sigma)

plot(x_grid, dx, type='l')

grade = 86
z_grade = (grade-mu)/sigma

abline(v=grade)

for(i in -3:3){
  abline(v=mu+i*sigma)
}

pnorm(grade, mean=mu, sd=sigma, lower.tail = TRUE)
qnorm(0.9, mean=mu, sd=sigma, lower.tail=TRUE)

pnorm(-25.2, mean=5, sd=10)
qnorm(0.01, mean=5, sd=10)
