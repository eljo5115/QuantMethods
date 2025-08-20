weight = NULL

for(i in 1:4){
  temp = rnorm(10, mean= i*5, sd=2)
  weight = c(weight, mean(temp))
}

print(c("weight vector:",weight))


sequence = seq(1,6,2)

print(c("sequence vector:", sequence))


x1 = 1:5
x2 = 6:10
x3 = 12:16

x = c(x1,x2,x3)

print(matrix(x,nrow=3))


y = cbind(x1,x2,x3)

print(y)