## Script that plots a Normal population against
##        two sampling distributions. One for n=10. 
##        The other for n=100

x = (-45000:65000)/1000
d = dnorm(x, mean = 5, sd = 10)
plot(x, d, type="l", bty="n", main = "", xlab = "X or X_BAR", ylab= "density", 
     xlim = c(-20,30), ylim=c(0,0.27))

## Plot a normal density (sampling distribution)
x = (-45000:65000)/1000
d = dnorm(x, mean = 5, sd = 10/sqrt(10))
lines(x, d, lty="dashed", col="blue")

x = (-45000:65000)/1000
d = dnorm(x, mean = 5, sd = 10/sqrt(50))
lines(x, d, lty="dotted", col="red")

legend(x=8, y=0.26, legend=c("pop (X)", "...n=10", "...n=50"), lty=c("solid", "dashed", "dotted"), col=c("black","blue","red"),bty="n", cex=0.8)
