### EC-374: Prethesis Seminar
### Data and Regression Calculations

## Clear:
rm(list = ls())
--------------------------------------------------------------------------------
## Import the Data:
--------------------------------------------------------------------------------
library(readr)
DOWJONES_Thesis_Data <- read_csv("/Users/alexlaudano/Desktop/EC374_Prethesis/Data/DOWJONES_Thesis_Data.csv")
GOLD_Thesis_Data     <- read_csv("/Users/alexlaudano/Desktop/EC374_Prethesis/Data/GOLD_Thesis_Data.csv")
COFFEE_Thesis_Data <- read_csv("/Users/alexlaudano/Desktop/EC374_Prethesis/Data/GOLD_Thesis_Data.csv")
CORN_Thesis_Data     <- read_csv("/Users/alexlaudano/Desktop/EC374_Prethesis/Data/CORN_Thesis_Data.csv")
COTTON_Thesis_Data   <- read_csv("/Users/alexlaudano/Desktop/EC374_Prethesis/Data/COTTON_Thesis_Data.csv")
SOYBEANS_Thesis_Data <- read_csv("/Users/alexlaudano/Desktop/EC374_Prethesis/Data/SOYBEANS_Thesis_Data.csv")
WHEAT_Thesis_Data <- read_csv("/Users/alexlaudano/Desktop/EC374_Prethesis/Data/WHEAT_Thesis_Data.csv")

--------------------------------------------------------------------------------
## Convert Corn, Cotton,Soybeans and Wheat Data from Daily to Monthly
--------------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(dplyr)

CORN_monthly <- CORN_Thesis_Data %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  arrange(Date) %>%
  group_by(yr = year(Date), mn = month(Date)) %>%
  summarise(Value = last(Value), .groups="drop") %>%
  mutate(Date = as.Date(paste(yr, mn, "01", sep="-")) + months(1) - days(1)) %>%
  select(Date, Value)
COTTON_monthly <- COTTON_Thesis_Data %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  arrange(Date) %>%
  group_by(yr = year(Date), mn = month(Date)) %>%
  summarise(Value = last(Value), .groups="drop") %>%
  mutate(Date = as.Date(paste(yr, mn, "01", sep="-")) + months(1) - days(1)) %>%
  select(Date, Value)
SOYBEANS_monthly <- SOYBEANS_Thesis_Data %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  arrange(Date) %>%
  group_by(yr = year(Date), mn = month(Date)) %>%
  summarise(Value = last(Value), .groups="drop") %>%
  mutate(Date = as.Date(paste(yr, mn, "01", sep="-")) + months(1) - days(1)) %>%
  select(Date, Value)
WHEAT_monthly <- WHEAT_Thesis_Data %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  arrange(Date) %>%
  group_by(yr = year(Date), mn = month(Date)) %>%
  summarise(Value = last(Value), .groups="drop") %>%
  mutate(Date = as.Date(paste(yr, mn, "01", sep="-")) + months(1) - days(1)) %>%
  select(Date, Value)
COFFEE_monthly <- COFFEE_Thesis_Data %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  arrange(Date) %>%
  group_by(yr = year(Date), mn = month(Date)) %>%
  summarise(Value = last(Value), .groups="drop") %>%
  mutate(Date = as.Date(paste(yr, mn, "01", sep="-")) + months(1) - days(1)) %>%
  select(Date, Value)
--------------------------------------------------------------------------------
## Remove Daily Data and Check What's Left
--------------------------------------------------------------------------------
rm(CORN_Thesis_Data, 
   COTTON_Thesis_Data, 
   SOYBEANS_Thesis_Data,
   WHEAT_Thesis_Data,
   COFFEE_Thesis_Data)
ls()

--------------------------------------------------------------------------------
## Rename Data and Drop Old Sets
--------------------------------------------------------------------------------
DowJones <- DOWJONES_Thesis_Data
Gold     <- GOLD_Thesis_Data
Corn     <- CORN_monthly
Cotton   <- COTTON_monthly
Soybeans <- SOYBEANS_monthly
Wheat <- WHEAT_monthly
Coffee <- COFFEE_monthly

rm(DOWJONES_Thesis_Data, 
   GOLD_Thesis_Data, 
   CORN_monthly, 
   COTTON_monthly,
   SOYBEANS_monthly,
   COFFEE_monthly)
ls()

--------------------------------------------------------------------------------
## Calculate Monthly Returns for Dow Jones
--------------------------------------------------------------------------------
DowJones <- DowJones %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  arrange(Date)
head(DowJones)
tail(DowJones)
DowJones <- DowJones %>%
  arrange(Date) %>%
  mutate(
    Value = as.numeric(Value),
    Return = (Value / lag(Value)) - 1)
DowJones <- DowJones %>%
  mutate(Return_pct = Return * 100)
summary(DowJones$Return_pct)

--------------------------------------------------------------------------------
## Calculate Monthly Returns for Gold
--------------------------------------------------------------------------------
Gold <- Gold %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  filter(Value > 0) %>%
  arrange(Date)
Gold <- Gold %>%
  group_by(yr = year(Date), mn = month(Date)) %>%
  summarise(Value = last(Value), .groups = "drop") %>%
  mutate(Date = as.Date(paste(yr, mn, "01", sep="-")) + months(1) - days(1)) %>%
  select(Date, Value)
Gold <- Gold %>%
  mutate(Return = (Value / lag(Value) - 1) * 100)
summary(Gold$Return)

--------------------------------------------------------------------------------
## Calculate Monthly Returns for Corn
--------------------------------------------------------------------------------
Corn <- Corn %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  filter(Value > 0) %>%
  arrange(Date)
Corn <- Corn %>%
  group_by(yr = year(Date), mn = month(Date)) %>%
  summarise(Value = last(Value), .groups = "drop") %>%
  mutate(Date = as.Date(paste(yr, mn, "01", sep="-")) + months(1) - days(1)) %>%
  select(Date, Value)
Corn <- Corn %>%
  mutate(Return = (Value / lag(Value) - 1) * 100)
summary(Corn$Return)

--------------------------------------------------------------------------------
## Calculate Monthly Returns for Cotton
--------------------------------------------------------------------------------
Cotton <- Cotton %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  filter(Value > 0) %>%
  arrange(Date)
Cotton <- Cotton %>%
  group_by(yr = year(Date), mn = month(Date)) %>%
  summarise(Value = last(Value), .groups = "drop") %>%
  mutate(Date = as.Date(paste(yr, mn, "01", sep="-")) + months(1) - days(1)) %>%
  select(Date, Value)
Cotton <- Cotton %>%
  mutate(Return = (Value / lag(Value) - 1) * 100)
summary(Cotton$Return)

--------------------------------------------------------------------------------
## Calculate Monthly Returns for Soybeans
--------------------------------------------------------------------------------
Soybeans <- Soybeans %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  filter(Value > 0) %>%
  arrange(Date)
Soybeans <- Soybeans %>%
  group_by(yr = year(Date), mn = month(Date)) %>%
  summarise(Value = last(Value), .groups = "drop") %>%
  mutate(Date = as.Date(paste(yr, mn, "01", sep="-")) + months(1) - days(1)) %>%
  select(Date, Value)
Soybeans <- Soybeans %>%
  mutate(Return = (Value / lag(Value) - 1) * 100)
summary(Soybeans$Return)

--------------------------------------------------------------------------------
## Calculate Monthly Returns for Wheat
--------------------------------------------------------------------------------
Wheat <- Wheat %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  filter(Value > 0) %>%
  arrange(Date)
Wheat <- Wheat %>%
  group_by(yr = year(Date), mn = month(Date)) %>%
  summarise(Value = last(Value), .groups = "drop") %>%
  mutate(Date = as.Date(paste(yr, mn, "01", sep="-")) + months(1) - days(1)) %>%
  select(Date, Value)
Wheat <- Wheat %>%
  mutate(Return = (Value / lag(Value) - 1) * 100)
summary(Wheat$Return)

--------------------------------------------------------------------------------
  ## Calculate Monthly Returns for Coffee
  --------------------------------------------------------------------------------
Coffee <- Coffee %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  filter(Value > 0) %>%
  arrange(Date)
Coffee <- Coffee %>%
  group_by(yr = year(Date), mn = month(Date)) %>%
  summarise(Value = last(Value), .groups = "drop") %>%
  mutate(Date = as.Date(paste(yr, mn, "01", sep="-")) + months(1) - days(1)) %>%
  select(Date, Value)
Coffee <- Coffee %>%
  mutate(Return = (Value / lag(Value) - 1) * 100)
summary(Coffee$Return)

--------------------------------------------------------------------------------
###  Monthly Momentum Regression Test
--------------------------------------------------------------------------------
# Function: runs r_t = α + β * r_{t-1} + ε_t
run_regression <- function(data, name) {
  model <- lm(Return ~ lag(Return), data = data)
  summary_stats <- summary(model)
  
  beta <- coef(summary_stats)[2, 1]
  tval <- coef(summary_stats)[2, 3]
  pval <- coef(summary_stats)[2, 4]
  r2   <- summary_stats$r.squared
  
  tibble(
    Asset = name,
    Beta = round(beta, 3),
    t_value = round(tval, 3),
    p_value = round(pval, 3),
    R2 = round(r2, 3)
  )
}

# Run for all assets and combine
results <- bind_rows(
  run_regression(DowJones, "Dow Jones"),
  run_regression(Gold, "Gold"),
  run_regression(Corn, "Corn"),
  run_regression(Cotton, "Cotton"),
  run_regression(Soybeans, "Soybeans"),
  run_regression(Wheat, "Wheat"),
  run_regression(Coffee, "Coffee")
)

# Display summary table
print(results)

--------------------------------------------------------------------------------
### Dow Jones Conditional Momentum Test 
--------------------------------------------------------------------------------
# Define threshold for a "strong up month"
threshold <- 10  # +10%

# Identify strong months
strong_months <- which(DowJones$Return_pct >= threshold)

# Collect returns of the *next* month after each strong up month
next_returns <- DowJones$Return_pct[strong_months + 1]

# Remove NAs (in case the last month has no next month)
next_returns <- next_returns[!is.na(next_returns)]

# Compare mean of next-month returns vs. unconditional mean
t_test <- t.test(next_returns, mu = mean(DowJones$Return_pct, na.rm = TRUE))

# Display results
cat("Dow Jones Strong-Up-Month Test\n")
cat("Threshold:", threshold, "%\n")
cat("Number of strong months:", length(strong_months), "\n")
cat("Next-month mean return:", round(mean(next_returns), 2), "%\n")
cat("Overall mean return:", round(mean(DowJones$Return_pct, na.rm=TRUE), 2), "%\n")
print(t_test)

--------------------------------------------------------------------------------
### Gold Top-Quartile Conditional Momentum Test 
--------------------------------------------------------------------------------
# Define top quartile threshold
threshold <- quantile(Gold$Return, 0.75, na.rm = TRUE)

# Identify top-quartile (strong) months
strong_months <- which(Gold$Return >= threshold)

# Collect returns of the *next* month
next_returns <- Gold$Return[strong_months + 1]
next_returns <- next_returns[!is.na(next_returns)]  # drop last NA if needed

# Unconditional mean
overall_mean <- mean(Gold$Return, na.rm = TRUE)

# Perform t-test: Is next-month mean > overall mean?
t_test <- t.test(next_returns, mu = overall_mean)

# Display results
cat("Gold Top-Quartile Continuation Test\n")
cat("Threshold (top 25%):", round(threshold, 2), "%\n")
cat("Number of strong months:", length(strong_months), "\n")
cat("Next-month mean return:", round(mean(next_returns), 2), "%\n")
cat("Overall mean return:", round(overall_mean, 2), "%\n")
print(t_test)

--------------------------------------------------------------------------------
### Corn Seasonal Efficiency Test (Quarterly)
--------------------------------------------------------------------------------
# Create a Quarter variable (1,2,3,4)
Corn <- Corn %>%
  mutate(Quarter = quarter(Date))

# Run one-way ANOVA to test equality of means across quarters
anova_model <- aov(Return ~ as.factor(Quarter), data = Corn)
summary(anova_model)

# Post-hoc test (optional) to see which quarters differ
TukeyHSD(anova_model)

# Compute mean returns by quarter for easy interpretation
quarter_means <- Corn %>%
  group_by(Quarter) %>%
  summarise(mean_return = mean(Return, na.rm = TRUE))
print(quarter_means)

--------------------------------------------------------------------------------
### Cotton 95th-Percentile Reversal Test 
--------------------------------------------------------------------------------
# Define the 95th-percentile threshold
threshold <- quantile(Cotton$Return, 0.95, na.rm = TRUE)

# Identify extreme up-months
extreme_months <- which(Cotton$Return >= threshold)

# Next-month returns
next_returns <- Cotton$Return[extreme_months + 1]
next_returns <- next_returns[!is.na(next_returns)]

# Overall mean
overall_mean <- mean(Cotton$Return, na.rm = TRUE)

# One-sample t-test: is next-month mean < overall mean?
t_test <- t.test(next_returns, mu = overall_mean, alternative = "less")

# Display results
cat("Cotton 95th-Percentile Reversal Test\n")
cat("Threshold (95th percentile):", round(threshold, 2), "%\n")
cat("Number of extreme months:", length(extreme_months), "\n")
cat("Next-month mean return:", round(mean(next_returns), 2), "%\n")
cat("Overall mean return:", round(overall_mean, 2), "%\n")
print(t_test)

--------------------------------------------------------------------------------
### Test Seasonality for Cotton
--------------------------------------------------------------------------------
Cotton <- Cotton %>%
  mutate(Quarter = quarter(Date))
# Run one-way ANOVA across quarters
anova_cotton <- aov(Return ~ as.factor(Quarter), data = Cotton)
summary(anova_cotton)

# Compute mean returns by quarter
cotton_quarter_means <- Cotton %>%
  group_by(Quarter) %>%
  summarise(mean_return = mean(Return, na.rm = TRUE))
print(cotton_quarter_means)

# (Optional) Post-hoc Tukey test to see which quarters differ
TukeyHSD(anova_cotton)

--------------------------------------------------------------------------------
### Test Seasonality for Soybeans
--------------------------------------------------------------------------------
Soybeans <- Soybeans %>%
mutate(Quarter = quarter(Date))
# Run one-way ANOVA across quarters
anova_soybeans <- aov(Return ~ as.factor(Quarter), data = Soybeans)
summary(anova_soybeans)

# Compute mean returns by quarter
soybeans_quarter_means <- Soybeans %>%
  group_by(Quarter) %>%
  summarise(mean_return = mean(Return, na.rm = TRUE))
print(soybeans_quarter_means)

mean(Wheat$Return, na.rm = TRUE)
mean(Soybeans$Return, na.rm = TRUE)



# (Optional) Post-hoc Tukey test to see which quarters differ
TukeyHSD(anova_cotton)

--------------------------------------------------------------------------------
### Test Seasonality for Wheat
--------------------------------------------------------------------------------
Wheat <- Wheat %>%
mutate(Quarter = quarter(Date))
# Run one-way ANOVA across quarters
anova_wheat <- aov(Return ~ as.factor(Quarter), data = Wheat)
summary(anova_wheat)

# Compute mean returns by quarter
wheat_quarter_means <- Wheat %>%
  group_by(Quarter) %>%
  summarise(mean_return = mean(Return, na.rm = TRUE))
print(wheat_quarter_means)

# (Optional) Post-hoc Tukey test to see which quarters differ
TukeyHSD(anova_wheat)

--------------------------------------------------------------------------------
### Test Seasonality for Coffee
--------------------------------------------------------------------------------
Coffee <- Coffee %>%
  mutate(Quarter = quarter(Date))
# Run one-way ANOVA across quarters
anova_coffee <- aov(Return ~ as.factor(Quarter), data = Coffee)
summary(anova_coffee)

# Compute mean returns by quarter
coffee_quarter_means <- Coffee %>%
  group_by(Quarter) %>%
  summarise(mean_return = mean(Return, na.rm = TRUE))
print(coffee_quarter_means)

# (Optional) Post-hoc Tukey test to see which quarters differ
TukeyHSD(anova_coffee)

--------------------------------------------------------------------------------
### Coffee Top-Quartile Conditional Momentum Test 
--------------------------------------------------------------------------------
# Define top quartile threshold
threshold <- quantile(Coffee$Return, 0.75, na.rm = TRUE)

# Identify top-quartile (strong) months
strong_months_coffee <- which(Coffee$Return >= threshold)

# Collect returns of the *next* month
next_returns <- Coffee$Return[strong_months + 1]
next_returns <- next_returns[!is.na(next_returns)]  # drop last NA if needed

# Unconditional mean
overall_mean <- mean(Coffee$Return, na.rm = TRUE)

# Perform t-test: Is next-month mean > overall mean?
t_test <- t.test(next_returns, mu = overall_mean)

# Display results
cat("Coffee Top-Quartile Continuation Test\n")
cat("Threshold (top 25%):", round(threshold, 2), "%\n")
cat("Number of strong months:", length(strong_months), "\n")
cat("Next-month mean return:", round(mean(next_returns), 2), "%\n")
cat("Overall mean return:", round(overall_mean, 2), "%\n")
print(t_test)


--------------------------------------------------------------------------------
### Test Seasonality for Dow Jones
--------------------------------------------------------------------------------
DowJones <- DowJones %>%
mutate(Quarter = quarter(Date))

# Run ANOVA for seasonality
anova_dow <- aov(Return ~ as.factor(Quarter), data = DowJones)
summary(anova_dow)

# Compute mean returns by quarter
dow_quarter_means <- DowJones %>%
  group_by(Quarter) %>%
  summarise(mean_return = mean(Return, na.rm = TRUE))
print(dow_quarter_means)

# Optional post-hoc test
TukeyHSD(anova_dow)

# Install if needed
# install.packages(c("quantmod", "dplyr"))

--------------------------------------------------------------------------------
### BITCOIN MONTHLY TESTS
--------------------------------------------------------------------------------
library(quantmod)
library(dplyr)

# 1. Download daily BTC prices from Yahoo
getSymbols("BTC-USD", src = "yahoo", from = "2014-01-01")

# 2. Convert to monthly format (use last closing price each month)
btc_monthly <- to.monthly(`BTC-USD`, indexAt = "lastof", OHLC = TRUE)

# 3. Compute simple monthly returns
BTC <- data.frame(
  Date = index(btc_monthly),
  Price = as.numeric(btc_monthly$`BTC-USD.Close`)
)

# 4. Compute monthly returns in %
BTC <- BTC %>%
  mutate(Return = 100 * (Price / lag(Price) - 1)) %>%
  filter(!is.na(Return))

head(BTC)

BTC$Quarter <- as.integer((as.integer(format(BTC$Date, "%m")) - 1) %/% 3 + 1)

anova_btc <- aov(Return ~ as.factor(Quarter), data = BTC)
summary(anova_btc)

aggregate(Return ~ Quarter, BTC, mean)

threshold <- quantile(BTC$Return, 0.75, na.rm = TRUE)
strong_months <- which(BTC$Return >= threshold)
next_months <- strong_months + 1
next_months <- next_months[next_months <= nrow(BTC)]
next_returns <- BTC$Return[next_months]
overall_mean <- mean(BTC$Return, na.rm = TRUE)
conditional_mean <- mean(next_returns, na.rm = TRUE)
excess <- conditional_mean - overall_mean
t_test <- t.test(next_returns, mu = overall_mean)
list(
  threshold = threshold,
  overall_mean = overall_mean,
  conditional_mean = conditional_mean,
  excess = excess,
  t_test = t_test
)

# 95th percentile threshold
threshold_95 <- quantile(BTC$Return, 0.95, na.rm = TRUE)
# Identify months in the top 5%
strong_months_95 <- which(BTC$Return >= threshold_95)
# Next months
next_idx_95 <- strong_months_95 + 1
next_idx_95 <- next_idx_95[next_idx_95 <= nrow(BTC)]
# Returns in the month after 95th percentile months
next_returns_95 <- BTC$Return[next_idx_95]
# Unconditional mean
overall_mean_95 <- mean(BTC$Return, na.rm = TRUE)
# Conditional mean
conditional_mean_95 <- mean(next_returns_95, na.rm = TRUE)
# Excess return
excess_95 <- conditional_mean_95 - overall_mean_95
# Significance test
t_test_95 <- t.test(next_returns_95, mu = overall_mean_95)
list(
  threshold_95 = threshold_95,
  count_strong_months = length(strong_months_95),
  conditional_mean_95 = conditional_mean_95,
  overall_mean_95 = overall_mean_95,
  excess_95 = excess_95,
  t_test_95 = t_test_95
)

# 90th percentile threshold
threshold_90 <- quantile(BTC$Return, 0.90, na.rm = TRUE)
# Identify 90th percentile months
strong_months_90 <- which(BTC$Return >= threshold_90)
# Next-month indices
next_idx_90 <- strong_months_90 + 1
next_idx_90 <- next_idx_90[next_idx_90 <= nrow(BTC)]
# Extract next-month returns
next_returns_90 <- BTC$Return[next_idx_90]
# Compute means
overall_mean_90 <- mean(BTC$Return, na.rm = TRUE)
conditional_mean_90 <- mean(next_returns_90, na.rm = TRUE)
# Excess
excess_90 <- conditional_mean_90 - overall_mean_90
# Significance test
t_test_90 <- t.test(next_returns_90, mu = overall_mean_90)
list(
  threshold_90 = threshold_90,
  count_strong_months = length(strong_months_90),
  conditional_mean_90 = conditional_mean_90,
  overall_mean_90 = overall_mean_90,
  excess_90 = excess_90,
  t_test_90 = t_test_90
)

wilcox.test(next_returns_90, mu = overall_mean_90)

--------------------------------------------------------------------------------
  ### BITCOIN WEEKLY TESTS
--------------------------------------------------------------------------------
library(quantmod)
library(dplyr)
library(xts)
# Daily BTC as xts
btc_xts <- getSymbols("BTC-USD", src="yahoo", from="2014-01-01", auto.assign = FALSE)

# Weekly bars (use default end-of-period; don't pass indexAt = "lastof")
btc_w <- xts::to.period(btc_xts, period = "weeks", name = "BTC")

# Build weekly return dataframe
BTCw <- data.frame(
  Date  = index(btc_w),
  Price = as.numeric(Cl(btc_w))
) %>%
  mutate(Return = 100 * (Price / dplyr::lag(Price) - 1)) %>%
  filter(!is.na(Return))

head(BTCw)

### 3. 95th percentile threshold
threshold_95 <- quantile(BTCw$Return, 0.95, na.rm = TRUE)

### 4. Identify strong weeks: top 5%
strong_weeks <- which(BTCw$Return >= threshold_95)

### 5. Next-week returns
next_ix <- strong_weeks + 1
next_ix <- next_ix[next_ix <= nrow(BTCw)]
next_returns_95 <- BTCw$Return[next_ix]

### 6. Overall vs conditional
overall_mean_95 <- mean(BTCw$Return, na.rm = TRUE)
conditional_mean_95 <- mean(next_returns_95, na.rm = TRUE)
excess_95 <- conditional_mean_95 - overall_mean_95

### 7. One-sample t-test: is next-week mean > overall mean?
t_test_95 <- t.test(next_returns_95, mu = overall_mean_95)

### 8. Print results
list(
  threshold_95 = threshold_95,
  n_strong_weeks = length(strong_weeks),
  n_next = length(next_returns_95),
  conditional_mean_95 = conditional_mean_95,
  overall_mean_95 = overall_mean_95,
  excess_95 = excess_95,
  t_test_95 = t_test_95
)

### 3. 75th percentile threshold
threshold_75 <- quantile(BTCw$Return, 0.75, na.rm = TRUE)

### 4. Identify top-quartile weeks
strong_weeks_75 <- which(BTCw$Return >= threshold_75)

### 5. Next-week returns
next_ix_75 <- strong_weeks_75 + 1
next_ix_75 <- next_ix_75[next_ix_75 <= nrow(BTCw)]
next_returns_75 <- BTCw$Return[next_ix_75]

### 6. Means and excess
overall_mean_75 <- mean(BTCw$Return, na.rm = TRUE)
conditional_mean_75 <- mean(next_returns_75, na.rm = TRUE)
excess_75 <- conditional_mean_75 - overall_mean_75

### 7. Significance test
t_test_75 <- t.test(next_returns_75, mu = overall_mean_75)

### 8. Output
list(
  threshold_75 = threshold_75,
  n_strong_weeks = length(strong_weeks_75),
  n_next = length(next_returns_75),
  conditional_mean_75 = conditional_mean_75,
  overall_mean_75 = overall_mean_75,
  excess_75 = excess_75,
  t_test_75 = t_test_75
)

### 90th Percentile Weekly Momentum Test (BTC)

# 90% threshold (median)
threshold_90 <- quantile(BTCw$Return, 0.90, na.rm = TRUE)

# All weeks above or equal to median
strong_weeks_90 <- which(BTCw$Return >= threshold_50)

# Next-week returns
next_ix_90 <- strong_weeks_90 + 1
next_ix_90 <- next_ix_90[next_ix_90 <= nrow(BTCw)]
next_returns_90 <- BTCw$Return[next_ix_90]

# Means
overall_mean_90 <- mean(BTCw$Return, na.rm = TRUE)
conditional_mean_90 <- mean(next_returns_90, na.rm = TRUE)
excess_90 <- conditional_mean_90 - overall_mean_90

# One-sample t-test
t_test_90 <- t.test(next_returns_90, mu = overall_mean_90)

list(
  threshold_90 = threshold_90,
  n_strong_weeks = length(strong_weeks_90),
  n_next = length(next_returns_90),
  conditional_mean_90 = conditional_mean_90,
  overall_mean_90 = overall_mean_90,
  excess_90 = excess_90,
  t_test_90 = t_test_90
)

### Wilcoxon signed-rank test on weekly BTC continuation (75th percentile)


wilcox_75 <- wilcox.test(next_returns_75, mu = median(BTCw$Return, na.rm = TRUE))

list(
  conditional_median_75 = median(next_returns_75),
  overall_median = median(BTCw$Return, na.rm = TRUE),
  excess_median = median(next_returns_75) - median(BTCw$Return, na.rm = TRUE),
  wilcoxon_75 = wilcox_75
)


--------------------------------------------------------------------------------
### FINAL RESULTS
--------------------------------------------------------------------------------
final_results <- data.frame(
  Asset = c("Dow Jones", "Gold", "Corn", "Cotton","Soybeans","Wheat","Coffee"),
  Test = c("Quarterly ANOVA", 
           "Top Quartile Momentum",
           "Quarterly ANOVA", 
           "Quarterly ANOVA",
           "Quarterly ANOVA", 
           "Quarterly ANOVA",
           "Top Quartile Momentum"),
  Statistic = c("F = 0.40",
                "t = 2.27",
                "F = 13.23", 
                "F = 3.65",
                "F = 9.37",
                "F = 8.03",
                "t = 2.27"),
  P_Value = c(
    0.756, 
    0.024, 
    "<0.001",
    0.013,
    "<0.001",
    "<0.001",
    0.0240
    ),
  Verdict = c("Efficient", "Inefficient", "Inefficient", "Inefficient","Inefficient", "Inefficient", "Inefficient")
)

print(final_results)

### Graph for Dow Jones
ggplot(DowJones, aes(x = Return)) +
  geom_histogram(bins = 40, fill = "#4682B4", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(Return, na.rm = TRUE)),
             color = "black", linetype = "solid", linewidth = 1) +
  labs(
    title = "Dow Jones: Efficient Baseline Distribution",
    x = "Monthly Return (%)", y = "Frequency"
  ) +
  theme_minimal(base_size = 13)



### Graph for Gold
threshold <- quantile(Gold$Return, 0.75, na.rm = TRUE)
Gold$Condition <- ifelse(Gold$Return >= threshold, "After Strong Month", "Normal")

ggplot(Gold, aes(x = lead(Return), fill = Condition)) +
  geom_histogram(bins = 35, alpha = 0.5, position = "identity", color = "white") +
  geom_vline(aes(xintercept = mean(lead(Return)[Condition == "After Strong Month"], na.rm = TRUE)),
             color = "#FFD700", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = mean(lead(Return)[Condition == "Normal"], na.rm = TRUE)),
             color = "gray40", linetype = "dotted", linewidth = 1) +
  labs(
    title = "Gold: Conditional Momentum Distribution",
    x = "Next-Month Return (%)", y = "Frequency"
  ) +
  scale_fill_manual(values = c("After Strong Month" = "#FFD700", "Normal" = "#bdbdbd")) +
  theme_minimal(base_size = 13)


### Graph for Corn
ggplot(Corn, aes(x = Return)) +
  geom_histogram(bins = 40, fill = "#228B22", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(Return, na.rm = TRUE)),
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Corn: Monthly Return Distribution",
    x = "Monthly Return (%)", y = "Frequency"
  ) +
  theme_minimal(base_size = 13)



library(ggplot2)

ineff_summary <- data.frame(
  Asset = c("Dow Jones", "Gold", "Corn", "Cotton","Soybeans","Wheat","Coffee","Bitcoin"),
  Conditional_Mean = c(0.65, 1.27, 2.15, 1.35, 1.61, 2.50, 1.26, 2.06),  # from your tests
  Overall_Mean     = c(0.65, 0.50, 0.05, 0.65, 0.53, 0.48, 0.49, 0.88)
) %>%
  mutate(
    Inefficiency = Conditional_Mean - Overall_Mean
  )

ggplot(ineff_summary, aes(x = reorder(Asset, Inefficiency), y = Inefficiency, fill = Asset)) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 0, color = "gray30", linewidth = 1) +
  coord_flip() +
  labs(
    title = "Conditional Mean Return Deviations",
    x = NULL,
    y = "Excess Return (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

ggplot(ineff_summary, aes(x = reorder(Asset, Inefficiency), 
                          y = Inefficiency, fill = Asset)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_hline(yintercept = 0, color = "gray30", linewidth = 1) +
  coord_flip() +
  labs(
    title = "The Ladder of Inefficiency",
    subtitle = "Conditional Mean Return Deviations by Asset Class",
    x = NULL,
    y = "Excess Return (%)"
  ) +
  scale_fill_manual(values = c(
    "Dow Jones" = "#999999",
    "Gold" = "#CBAEFF",
    "Corn" = "#FF6F61",
    "Cotton" = "#9ACD32",
    "Soybeans" = "blue",
    "Wheat" = "orange",
    "Coffee" = "brown",
    "Bitcoin" = "purple"
  )) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12)
  )



