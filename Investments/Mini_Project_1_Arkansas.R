# ----------
# Group: Arkansas
# MSBC 5031 - Investment Management
# Mini Project 1
# -----------

setwd("/Users/eli/noticloud/Documents/GitHub/QuantMethods/Investments")
ff <- read.table("french_portfolios_2025.txt")

# Plot Large Hi BM to Large Lo BM
names(ff) = c("DATE","S.L", "S.M", "S.H", "B.L", "B.M", "B.H")

ff$DATE <- as.character(ff$DATE)
ff$DATE <- paste(ff$DATE, "01", sep = "")
ff$DATE = as.Date(ff$DATE, format="%Y%m%d")

gross_ret = function(vec){
  gross_ret = 1+(vec/100)
  return(gross_ret)
}

gr <- data.frame(ff)

## Defining variables for gross returns ##
gr$big_low = gross_ret(ff$B.L)
gr$big_mid = gross_ret(ff$B.M)
gr$big_hi = gross_ret(ff$B.H)

gr$sm_low = gross_ret(ff$S.L)
gr$sm_mid = gross_ret(ff$S.M)
gr$sm_hi = gross_ret(ff$S.H)

par(mfrow=c(1,2))
plot(gr$DATE, cumprod(gr$big_hi), type="l", main="Large Firm Returns", sub="Black is high B/M, Blue is low B/M", xlab="Date", ylab="Cumulative Return")
lines(gr$DATE, cumprod(gr$big_low), col="blue")

plot(gr$DATE, cumprod(gr$sm_hi), type="l", main="Small Firm Returns", sub="Black is high B/M, Blue is low B/M", xlab="Date", ylab="Cumulative Return")
lines(gr$DATE, cumprod(gr$sm_low), col="blue", lty="dashed")

# arithmetic means
am_sl = mean(ff$S.L) # 0.974
am_sh = mean(ff$S.H) # 1.418
am_bl =mean(ff$B.L) # 0.956
am_bh = mean(ff$B.H) # 1.206

# standard deviation
sd_sl = sd(ff$S.L) # 7.447
sd_sh = sd(ff$S.H) # 8.095
sd_bl = sd(ff$B.L) # 5.275
sd_bh = sd(ff$B.H) # 7.095

# geometric means
gm_mean = function(a){prod(a)^(1/length(a))}
gm_sl = gm_mean(gr$sm_low) #1.007
gm_sh = gm_mean(gr$sm_hi) # 1.011
gm_bl = gm_mean(gr$big_low) # 1.008
gm_bh = gm_mean(gr$big_high) # 1

# approximate geo means
agm_sl = gm_sl - 0.5*sd_sl**2 # -26.72
agm_sh = gm_sh - 0.5*sd_sh**2 # -31.75
agm_bl = gm_bl - 0.5*sd_bl**2 # -12.90
agm_bh = gm_bh - 0.5*sd_bh**2 # -24.17

t.test(gr$sm_low, gr$big_low, paired= TRUE)
t.test(gr$sm_hi, gr$big_hi, paired = TRUE)

hist(gr$sm_low,breaks=100)
