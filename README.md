# Seasonality & Momentum: Persistent Inefficiencies in Commodities and Crypto

This repository contains the research note, figures, and R code for my independent study testing weak-form EMH across commodities, equities, and Bitcoin.

Using more than 20 years of historical returns and R (tidyverse, ggplot2), I tested for:

- Commodity seasonality using ANOVA
- Momentum continuation using one-sample t-tests
- Bitcoin median weekly momentum using a Wilcoxon signed-rank test
- Dow Jones as control (no inefficiency)

## Files
- analysis.R (R script for data cleaning, return calculation, and tests)
- research_note.pdf (short formatted research note with charts)
