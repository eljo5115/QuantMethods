# ------------- 
# Author: Eli Jordan
# Investment Case 9: Multifactor Model
# ------------- 

# Ensure necessary packages are installed and loaded
if (!require(quantmod)) {
  install.packages("quantmod")
  library(quantmod)
}
if (!require(lubridate)) {
  install.packages("lubridate")
  library(lubridate)
}

# --- Part 1: Data Gathering and Cleaning ---

# Set the start date to 60 months (5 years) ago
start_date <- Sys.Date() %m-% months(60)
end_date <- Sys.Date()

# Tickers for the funds and companies to be analyzed
tickers <- c("JGMAX", "BFOCX", "JBLU", "SLB")

# Download monthly stock data from Yahoo Finance
getSymbols(tickers, src = "yahoo", from = start_date, to = end_date, periodicity = "monthly")

# Download monthly crude oil price data (WTI) from FRED
getSymbols("DCOILWTICO", src = "FRED", from = start_date, to = end_date)


# IMPORTANT: Set your working directory to the project's root folder if you are running this line-by-line
setwd("/Users/eli/noticloud/Documents/GitHub/QuantMethods") 
ff_path <- file.path("Investments", "data", "F-F.CSV")
ff_data <- read.csv(ff_path, header = TRUE)

# Rename the first column which contains dates in YYYYMM format
names(ff_data)[1] <- "yearmonth"

# Convert 'yearmonth' to a Date object
ff_data$Date <- as.Date(paste0(ff_data$yearmonth, "01"), format = "%Y%m%d")

# Convert factor returns from percentage to decimal
factor_cols <- c("Mkt.RF", "SMB", "HML", "RF")
for (col in factor_cols) {
  ff_data[, col] <- as.numeric(as.character(ff_data[, col])) / 100
}

# Create an xts object for merging
ff_xts <- xts(ff_data[, factor_cols], order.by = ff_data$Date)

# Remove NA rows
ff_xts <- na.omit(ff_xts)


# --- Calculate Monthly Returns ---

# Calculate monthly returns for the stocks
# Using Ad(Cl(x)) to get adjusted-close returns
jgmax_ret <- monthlyReturn(Ad(JGMAX))
bfocx_ret <- monthlyReturn(Ad(BFOCX))
jblu_ret <- monthlyReturn(Ad(JBLU))
slb_ret <- monthlyReturn(Ad(SLB))

# For FRED data (like oil), it's a price series. We need to calculate returns manually.
# First, fill in missing values (weekends, holidays) using last observation carried forward
oil_prices <- na.locf(DCOILWTICO)
# Now calculate monthly returns
oil_ret <- monthlyReturn(oil_prices)

# Rename columns to be clear and consistent
names(jgmax_ret) <- "JGMAX.ret"
names(bfocx_ret) <- "BFOCX.ret"
names(jblu_ret) <- "JBLU.ret"
names(slb_ret) <- "SLB.ret"
names(oil_ret) <- "OIL.ret"


# --- Merge All Data ---

# Merge all return series and Fama-French factors into a single xts object
# This automatically aligns all data by date
all_data <- merge(jgmax_ret, bfocx_ret, jblu_ret, slb_ret, oil_ret, ff_xts, all = FALSE)

# Remove any rows with NA values that might have resulted from the merge
all_data <- na.omit(all_data)

# Take the most recent 60 months of complete data
all_data <- last(all_data, "60 months")


# --- Calculate Excess Returns ---

# Calculate excess returns for each asset by subtracting the risk-free rate
all_data$JGMAX.xs <- all_data$JGMAX.ret - all_data$RF
all_data$BFOCX.xs <- all_data$BFOCX.ret - all_data$RF
all_data$JBLU.xs <- all_data$JBLU.ret - all_data$RF
all_data$SLB.xs <- all_data$SLB.ret - all_data$RF
all_data$OIL.xs <- all_data$OIL.ret - all_data$RF


# --- Part 2: Regression Analysis ---

# Convert xts object to a data frame for use with lm()
data_df <- data.frame(date = index(all_data), coredata(all_data))

cat("--- Analyzing Active Fund: JGMAX ---", "\n\n")
# Regress excess return of JGMAX on the Fama-French 3 factors
jgmax_lm <- lm(JGMAX.xs ~ Mkt.RF + SMB + HML, data = data_df)
print(summary(jgmax_lm))
cat("\n\n")


cat("--- Analyzing Passive Fund: BFOCX ---", "\n\n")
# Regress excess return of BFOCX on the Fama-French 3 factors
bfocx_lm <- lm(BFOCX.xs ~ Mkt.RF + SMB + HML, data = data_df)
print(summary(bfocx_lm))
cat("\n\n")


cat("--- Analyzing Oil-Sensitive Companies: SLB and JBLU ---", "\n\n")
# For SLB and JBLU, we add the excess return of crude oil as a fourth factor

cat("--- SLB (Schlumberger) Regression ---", "\n")
slb_lm <- lm(SLB.xs ~ Mkt.RF + SMB + HML + OIL.xs, data = data_df)
print(summary(slb_lm))
cat("\n\n")


cat("--- JBLU (JetBlue) Regression ---", "\n")
jblu_lm <- lm(JBLU.xs ~ Mkt.RF + SMB + HML + OIL.xs, data = data_df)
print(summary(jblu_lm))
cat("\n\n")

cat("--- Script Finished --- \n")
cat("The regression summaries above provide the statistics needed to answer the interpretation questions for each part of the assignment.\n")
cat("Alpha is the 'Intercept' in the regression summary.\n")
cat("Betas for the factors (Mkt.RF, SMB, HML, OIL.xs) are the 'Estimate' values for each variable.\n")
