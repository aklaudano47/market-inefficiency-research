# Seasonality & Momentum: Persistent Inefficiencies in Commodities and Crypto

This repository contains the research note, figures, and R code for my independent study testing weak-form EMH across commodities, equities, and Bitcoin.

Using more than 20 years of historical returns and R (tidyverse, ggplot2), I tested for:

- Commodity seasonality using ANOVA
- Momentum continuation using one-sample t-tests
- Bitcoin median weekly momentum using a Wilcoxon signed-rank test
- Dow Jones as control (no inefficiency)

## Files
- Data&Regressions_working.R (R script for data cleaning, return calculation, and tests)
- seasonality_momentum_research_note.pdf (short formatted research note with charts)

## Results (Summary)
- Corn, Wheat, and Soybeans had statistically significant seasonal effects
- Coffee and Gold displayed momentum continuation
- Bitcoin weekly returns showed median momentum (Wilcoxon confirmed)
- Dow Jones showed no inefficiency (efficient control asset)

## View Files
- [Research Note (PDF)](seasonality_momentum_research_note.pdf)
- [R Script](Data&Regressions_working.R)
