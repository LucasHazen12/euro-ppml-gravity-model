# euro-ppml-gravity-model
R code for my paper titled: The Euro and Bilateral Trade:  Evidence from Staggered Adoption, 1995-2020

# Euro Currency Union and Bilateral Trade: Evidence from Staggered Adoption, 1995–2020

## Overview
This project estimates the causal effect of Euro adoption on bilateral trade using a 
differences-in-differences design with a saturated fixed effects structure. Using the 
CEPII Gravity Database across 1.4 million country-pair-year observations, I find no 
significant average effect of Euro adoption — but uncover meaningful heterogeneity: 
pairs involving later-joining economies experienced a ~16% trade increase in the early 
period (2000–2009), while founding member pairs showed no effect at any point. Both 
effects disappeared after 2010, coinciding with the Eurozone sovereign debt crisis.

## Research Question
Does adopting a common currency increase bilateral trade between member countries, 
and does this effect vary by member cohort and time period?

## Data
- **Source:** CEPII Gravity Database (2022 release)
- **Coverage:** 1995–2020, ~1.4 million country-pair-year observations after filtering
- **Trade flows:** UN COMTRADE (destination-reported, prioritized)
- **Key variables:** Bilateral trade (USD), BothEuro indicator, RTA controls, 
  exporter/importer GDP, bilateral distance

## Methods
- **Primary estimator:** Poisson Pseudo-Maximum Likelihood (PPML) with saturated 
  fixed effects — chosen for robustness to heteroskedasticity and correct handling 
  of zero-trade observations (47% of sample)
- **Fixed effects structure:** Exporter-year × importer-year × country-pair 
  (absorbs time-varying country characteristics and all time-invariant bilateral factors)
- **Identification:** Within-pair variation over time — each country pair compared 
  to itself before and after both members adopt the Euro
- **Robustness checks:**
  - Log-OLS for comparison with prior literature (Rose 2000)
  - Exclusion of Greece pairs
  - Decomposition by founding members vs. later joiners × early/late time periods
  - Year-by-year event study (relative to 1998 base year)

## Key Results
| Specification | Estimator | Coefficient | Interpretation |
|---|---|---|---|
| Baseline BothEuro | PPML | -0.036 (ns) | No average effect |
| BothEuro Early (2000–2009) | OLS | 0.094* | ~9.9% increase |
| BothEuro Late (2010–2020) | OLS | -0.014 (ns) | No effect |
| Joiners Early | OLS | 0.152** | ~16.4% increase |
| Joiners Late | OLS | 0.009 (ns) | Effect disappeared |
| Founders Early | OLS | 0.003 (ns) | No effect |

## Files
- `euro_gravity.R` — Full analysis script: data loading, variable construction, 
  baseline regressions, time heterogeneity, robustness checks, and all visualizations
- `euro_summary_stats.html` — Summary statistics table
- `euro_baseline_table.html` — Table 1: Baseline OLS vs. PPML results
- `euro_time_heterogeneity.html` — Table 2: Early vs. late period decomposition
- `euro_founders_joiners_table.html` — Table 4: Founders vs. joiners × time
- `euro_robustness_comparison.html` — Full robustness comparison (Table 5)
- `euro_trade_trends_clean.png` — Figure 1: Euro vs. never-Euro trade trends
- `euro_event_study.png` — Figure 2: Year-by-year event study
- `euro_coef_plot_founders_joiners.png` — Figure 3: Coefficient plot by cohort

## R Packages
```r
tidyverse, fixest, modelsummary, janitor, ggplot2, scales, here
```

## How to Run
1. Download the CEPII Gravity Database (Gravity_V202211.rds) from http://www.cepii.fr
2. Update the file path in Section 1 of `euro_gravity.R`
3. Run the full script sequentially — outputs are saved to `~/Desktop/` by default
