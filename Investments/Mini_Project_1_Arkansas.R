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
gr$big_low = gross_ret(ff$B.L) #low B/M: growth
gr$big_mid = gross_ret(ff$B.M)
gr$big_hi = gross_ret(ff$B.H) # high B/m: value

gr$sm_low = gross_ret(ff$S.L) # low b/m: growth
gr$sm_mid = gross_ret(ff$S.M)
gr$sm_hi = gross_ret(ff$S.H) # high b/m: value

# calculate and store 
dd <- data.frame(ff)
dd$big_low = (cummax(gr$big_low) - gr$big_low)/gr$big_low
dd$big_mid = (cummax(gr$big_mid) - gr$big_mid)/gr$big_mid
dd$big_hi = (cummax(gr$big_hi) - gr$big_hi)/gr$big_hi

dd$sm_low = (cummax(gr$sm_low) - gr$sm_low)/gr$sm_low
dd$sm_mid = (cummax(gr$sm_mid) - gr$sm_mid)/gr$sm_mid
dd$sm_hi = (cummax(gr$sm_hi) - gr$sm_hi)/gr$sm_hi

max(dd$big_low)
max(dd$big_hi)

max(dd$sm_low)
max(dd$sm_hi)

par(mfrow=c(1,2))
plot(gr$DATE, cumprod(gr$sm_hi), type="l", main="Growth Firm Returns", sub="Black is high B/M, Blue is low B/M", xlab="Date", ylab="Cumulative Return")
lines(gr$DATE, cumprod(gr$big_hi), col="blue")

plot(gr$DATE, cumprod(gr$big_low), type="l", main="Value Firm Returns", xlab="Date", ylab="Cumulative Return",sub="Black is high B/M, Blue is low B/M")
lines(gr$DATE, cumprod(gr$sm_low), col="blue", lty="dashed")

# arithmetic means
am_sl = mean(ff$S.L) # 0.974
am_sh = mean(ff$S.H) # 1.418
am_bl = mean(ff$B.L) # 0.956
am_bh = mean(ff$B.H) # 1.206

# standard deviation
sd_sl = sd(ff$S.L) # 7.447
sd_sh = sd(ff$S.H) # 8.095
sd_bl = sd(ff$B.L) # 5.275
sd_bh = sd(ff$B.H) # 7.095

# geometric means
# average growth rate
gm_mean = function(a){prod(a)^(1/length(a))}
geo_m_mean_sl = gm_mean(gr$sm_low) #1.007 = .7% growth YoY
geo_m_mean_sh = gm_mean(gr$sm_hi) # 1.011 = 1.1% growth YoY
geo_m_mean_bl = gm_mean(gr$big_low) # 1.008 = 0.8% growth YoY
geo_m_mean_bh = gm_mean(gr$big_high) # 1 = 0% growth YoY


# approximate geo means
# approximate geometric mean =  arithmetic mean - 1/2*sd^2
approx_gm_sl = am_sl - 0.5*sd_sl**2 # -26.72
approx_gm_sh = am_sh - 0.5*sd_sh**2 # -31.75
approx_gm_bl = am_bl - 0.5*sd_bl**2 # -12.90
approx_gm_bh = am_bh - 0.5*sd_bh**2 # -24.17

t.test(gr$sm_low, gr$big_low, paired= TRUE) # p-value 0.88, reject
t.test(gr$sm_hi, gr$big_hi, paired = TRUE) # p-value 0.023, fail to reject

hist(ff$S.L,breaks=100)
