# -----------
# Author: Eli Jordan
# Implementing Markowitz Project
# Script to get the monthly data
# -----------

# install quantmod if not already installed
if(!require(quantmod)){
  install.packages("quantmod")
}
library(quantmod)

# stocks we are Markowitzing
stocksToAnalyze <- c("^GSPC","MMM","A","SLV")
# get monthly data, periodicity=monthly
getSymbols(stocksToAnalyze, src="yahoo",from=as.Date("2002-01-01"),to=Sys.Date(),periodicity="monthly")

# create returns dataframes from adjusted prices
gspc <- as.data.frame(Delt(GSPC$GSPC.Adjusted,k=1))
mmm <- as.data.frame(Delt(MMM$MMM.Adjusted,k=1))
a <- as.data.frame(Delt(A$A.Adjusted,k=1))
slv <- as.data.frame(Delt(SLV$SLV.Adjusted,k=1))

# rename indices to dates
rownames(gspc) <- as.Date(rownames(gspc))
rownames(mmm) <- as.Date(rownames(mmm))
rownames(a) <- as.Date(rownames(a))
rownames(slv) <- as.Date(rownames(slv))

# rename columns for each asset returns
colnames(gspc) <- "GSPC_ret"
colnames(mmm) <- "MMM_ret"
colnames(a) <- "A_ret"
colnames(slv) <- "SLV_ret"

# create Date column
gspc$Date <- rownames(gspc)
mmm$Date <- rownames(mmm)
a$Date <- rownames(a)
slv$Date <- rownames(slv)

# merge into one dataframe
merged_dat <- data.frame(NA)
merged_dat <- merge(gspc,mmm,by="Date")
merged_dat <- merge(merged_dat,a,by="Date")
# right join on SLV to retain data from 2002-2006, before SLV started
merged_dat <- merge(merged_dat,slv,by="Date",all.x=TRUE)
# drop 1st row of NAs
merged_dat <- merged_dat[2:nrow(merged_dat),]
merged_dat$SLV_ret[is.na(merged_dat$SLV_ret)] <- 0

# write merged data to csv
write.csv(merged_dat,"Implementing_Markowitz_data.csv")
