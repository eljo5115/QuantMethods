# "state_return_means": [
#     0.0026145305570155625,
#     0.08061474840437506,
#     -0.04011255846906809
# ],
# "state_return_stds": [
#     0.028182542638737575,
#     0.06345441011950356,
#     0.06780459843745007
# ],


curve(dnorm(x,mean=0.0026145305570155625,sd=0.028182542638737575),
col="red",
lwd=2,
add=FALSE,
xlim=c(-0.07,0.07)
,main="State 1",
xlab="Returns",
ylab="Density"
)
abline(v=0.0026145305570155625,col="red",lwd=2)

curve(dnorm(x,mean=0.08061474840437506,sd=0.06345441011950356),
col="blue",
lwd=2,
add=FALSE,
xlim=c(-0.1,0.3)
,main="State 2",
xlab="Returns",
ylab="Density"
)
abline(v=0.08061474840437506,col="blue",lwd=2)

curve(dnorm(x,mean=-0.04011255846906809,sd=0.0678045984374500),
col="green",
lwd=2,
add=FALSE,
xlim=c(-0.3,0.2)
,main="State 3",
xlab="Returns",
ylab="Density"
)
abline(v=-0.04011255846906809,col="green",lwd=2)
