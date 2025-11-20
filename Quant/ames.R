setwd("C:/Users/elijo/QuantMethods/Quant/data")
load("ames.Rdata")

ames_lm <- lm(ames$SalePrice~ames$Year.Built)

summary(ames_lm)

cor(ames$Year.Built,ames$SalePrice)
