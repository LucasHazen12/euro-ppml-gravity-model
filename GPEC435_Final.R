#################################
# Euro Currency Union and Trade
# GPEC 435 Final Project
# Lucas Hazen
#################################

# Clear environment
rm(list = ls())

# Load packages
library(tidyverse)
library(here)
library(fixest)
library(modelsummary)
library(janitor)
library(ggplot2)
library(scales)

#################################
# Section 1: Load and prepare data
#################################

cat("\n========================================\n")
cat("SECTION 1: LOAD DATA\n")
cat("========================================\n\n")

# Load CEPII Gravity data
gravity <- read_rds("~/Desktop/R Materials/Gravity_V202211.rds")

# Filter to 1995-2020
cat("Filtering to 1995-2020\n")
gravity <- gravity %>%
  filter(year >= 1995 & year <= 2020)

cat("Observations after year filter:", nrow(gravity), "\n")

# Rename variables for clarity
gravity <- gravity %>%
  rename(importer_iso3 = iso3_d, exporter_iso3 = iso3_o)

# Filter to existing countries only
cat("Filtering to existing countries\n")
gravity <- subset(gravity, country_exists_d == 1 & country_exists_o == 1)

cat("Observations after country filter:", nrow(gravity), "\n")

# Remove self-trade
cat("Removing self-trade\n")
gravity <- gravity %>%
  filter(importer_iso3 != exporter_iso3)

cat("Observations after removing self-trade:", nrow(gravity), "\n\n")

#################################
# Section 2: Create variables
#################################

cat("\n========================================\n")
cat("SECTION 2: CREATE VARIABLES\n")
cat("========================================\n\n")

# Create BothEuro indicator using official Euro adoption dates
cat("Creating BothEuro indicator using adoption dates\n")

# Euro adoption dates
euro_adopters <- tribble(
  ~iso3, ~euro_year,
  "AUT", 1999,  # Austria
  "BEL", 1999,  # Belgium
  "FIN", 1999,  # Finland
  "FRA", 1999,  # France
  "DEU", 1999,  # Germany
  "IRL", 1999,  # Ireland
  "ITA", 1999,  # Italy
  "LUX", 1999,  # Luxembourg
  "NLD", 1999,  # Netherlands
  "PRT", 1999,  # Portugal
  "ESP", 1999,  # Spain
  "GRC", 2001,  # Greece
  "SVN", 2007,  # Slovenia
  "CYP", 2008,  # Cyprus
  "MLT", 2008,  # Malta
  "SVK", 2009,  # Slovakia
  "EST", 2011,  # Estonia
  "LVA", 2014,  # Latvia
  "LTU", 2015,  # Lithuania
  "HRV", 2023   # Croatia (outside sample period)
)

# Create indicator for whether each country has adopted Euro by year t
gravity <- gravity %>%
  left_join(euro_adopters %>% rename(exporter_iso3 = iso3, euro_year_exp = euro_year), 
            by = "exporter_iso3") %>%
  left_join(euro_adopters %>% rename(importer_iso3 = iso3, euro_year_imp = euro_year), 
            by = "importer_iso3") %>%
  mutate(
    euro_exporter = ifelse(!is.na(euro_year_exp) & year >= euro_year_exp, 1, 0),
    euro_importer = ifelse(!is.na(euro_year_imp) & year >= euro_year_imp, 1, 0),
    BothEuro = ifelse(euro_exporter == 1 & euro_importer == 1, 1, 0)
  )

cat("BothEuro distribution:\n")
print(table(gravity$BothEuro, gravity$year))
cat("\n")

# Check how many Euro pairs there are
cat("Number of country pairs with both in Euro:\n")
print(sum(gravity$BothEuro == 1))
cat("\n")

# Create time period indicators for heterogeneity analysis
gravity <- gravity %>%
  mutate(
    Early = ifelse(year >= 2000 & year <= 2009, 1, 0),
    Late = ifelse(year >= 2010 & year <= 2020, 1, 0),
    Post2010 = ifelse(year >= 2010, 1, 0)
  )

# Create founder indicator
# Founders adopted in 1999: AUT, BEL, FIN, FRA, DEU, IRL, ITA, LUX, NLD, PRT, ESP
founders <- c("AUT", "BEL", "FIN", "FRA", "DEU", "IRL", "ITA", "LUX", "NLD", "PRT", "ESP")

gravity <- gravity %>%
  mutate(
    exporter_founder = ifelse(exporter_iso3 %in% founders, 1, 0),
    importer_founder = ifelse(importer_iso3 %in% founders, 1, 0),
    BothFounders = ifelse(BothEuro == 1 & exporter_founder == 1 & importer_founder == 1, 1, 0),
    AtLeastOneJoiner = ifelse(BothEuro == 1 & (exporter_founder == 0 | importer_founder == 0), 1, 0)
  )

# Create interaction for time heterogeneity
gravity <- gravity %>%
  mutate(
    BothEuro_Early = BothEuro * Early,
    BothEuro_Late = BothEuro * Late
  )

# Create RTA indicator
# For EU members, use eu_o and eu_d
gravity <- gravity %>%
  mutate(
    RTA = ifelse(!is.na(fta_wto) & fta_wto == 1, 1, 0)
  )

# Create panel identifiers
gravity$imp_exp_pair <- paste(gravity$importer_iso3, gravity$exporter_iso3, sep = "_")
gravity$exp_year <- paste(gravity$exporter_iso3, gravity$year, sep = "_")
gravity$imp_year <- paste(gravity$importer_iso3, gravity$year, sep = "_")

# Create log variables
cat("Creating log-transformed variables\n")

# Use tradeflow_comtrade_d (destination-reported, which is the most reliable)
gravity <- gravity %>%
  mutate(
    # Trade flow (use combined importer/exporter data)
    trade_combined = ifelse(!is.na(tradeflow_comtrade_d), 
                            tradeflow_comtrade_d, 
                            tradeflow_comtrade_o),
    
    # Log trade (for OLS)
    log_trade = ifelse(trade_combined > 0 & is.finite(log(trade_combined)), 
                       log(trade_combined), NA),
    
    # Trade for PPML (replace NA with 0)
    trade_ppml = ifelse(is.na(trade_combined), 0, trade_combined),
    
    # Log GDP
    log_gdp_o = ifelse(is.finite(log(gdp_o)), log(gdp_o), NA),
    log_gdp_d = ifelse(is.finite(log(gdp_d)), log(gdp_d), NA),
    
    # Log distance
    log_dist = ifelse(is.finite(log(dist)), log(dist), NA)
  )

cat("Variable creation complete\n\n")

#################################
# Section 3: Summary statistics
#################################

cat("\n========================================\n")
cat("SECTION 3: SUMMARY STATISTICS\n")
cat("========================================\n\n")

# Select key variables for summary stats
summary_vars <- gravity %>%
  select(trade_combined, log_trade, gdp_o, gdp_d, dist, 
         BothEuro, RTA, contig, comlang_off) %>%
  summary()

print(summary_vars)

# Create summary statistics table
sumstats <- gravity %>%
  select(trade_combined, gdp_o, gdp_d, dist, BothEuro, RTA) %>%
  datasummary_skim(output = "data.frame")

print(sumstats)

# Save summary stats table

# Create summary statistics table with more decimal places
datasummary_skim(
  gravity %>% select(trade_combined, gdp_o, gdp_d, dist, BothEuro, RTA),
  histogram = FALSE,
  fmt = 3,  # Show 3 decimal places instead of default rounding
  output = "~/Desktop/euro_summary_stats.html",
  title = "Summary Statistics"
)

cat("Summary statistics saved\n\n")

# Create cleaner summary stats table
library(tidyverse)

summary_clean <- gravity %>%
  select(trade_combined, gdp_o, gdp_d, dist, BothEuro, RTA) %>%
  rename(
    "Bilateral Trade (USD)" = trade_combined,
    "Exporter GDP (USD)"    = gdp_o,
    "Importer GDP (USD)"    = gdp_d,
    "Distance (km)"         = dist,
    "Both Euro"             = BothEuro,
    "RTA"                   = RTA
  )

datasummary_skim(
  summary_clean,
  histogram = FALSE,
  fmt = function(x) formatC(x, format = "f", digits = 1, big.mark = ","),
  output = "~/Desktop/euro_summary_stats_clean.html",
  title = "Table S1: Summary Statistics"
)

#################################
# Section 4: Baseline regressions
#################################

cat("\n========================================\n")
cat("SECTION 4: BASELINE REGRESSIONS\n")
cat("========================================\n\n")

# OLS Baseline
cat("Running OLS baseline\n")
ols_baseline <- feols(log_trade ~ BothEuro + RTA | 
                        imp_year + exp_year + imp_exp_pair,
                      data = gravity,
                      cluster = ~imp_exp_pair)

# PPML Baseline
cat("Running PPML baseline\n")
ppml_baseline <- fepois(trade_ppml ~ BothEuro + RTA | 
                          imp_year + exp_year + imp_exp_pair,
                        data = gravity,
                        cluster = ~imp_exp_pair)

cat("\n=== BASELINE RESULTS ===\n")
summary(ols_baseline)
summary(ppml_baseline)

#################################
# Section 5: Time Heterogeneity
#################################

cat("\n========================================\n")
cat("SECTION 5: TIME HETEROGENEITY\n")
cat("========================================\n\n")

# OLS with time interactions
cat("Running OLS with time heterogeneity\n")
ols_time <- feols(log_trade ~ BothEuro_Early + BothEuro_Late + RTA | 
                    imp_year + exp_year + imp_exp_pair,
                  data = gravity,
                  cluster = ~imp_exp_pair)

# PPML with time interactions
cat("Running PPML with time heterogeneity\n")
ppml_time <- fepois(trade_ppml ~ BothEuro_Early + BothEuro_Late + RTA | 
                      imp_year + exp_year + imp_exp_pair,
                    data = gravity,
                    cluster = ~imp_exp_pair)

cat("\n=== TIME HETEROGENEITY RESULTS ===\n")
summary(ols_time)
summary(ppml_time)

#################################
# Section 6: Regression tables
#################################

cat("\n========================================\n")
cat("SECTION 6: CREATE REGRESSION TABLES\n")
cat("========================================\n\n")

# Table 1: Baseline results
baseline_models <- list(
  "OLS" = ols_baseline,
  "PPML" = ppml_baseline
)

modelsummary(baseline_models,
             stars = TRUE,
             gof_omit = "AIC|BIC|RMSE|Std.Errors",
             output = "~/Desktop/euro_baseline_table.html",
             title = "Table 1: Baseline Euro Effects on Trade")

# Table 2: Time heterogeneity
time_models <- list(
  "OLS" = ols_time,
  "PPML" = ppml_time
)

modelsummary(time_models,
             stars = TRUE,
             gof_omit = "AIC|BIC|RMSE|Std.Errors",
             output = "~/Desktop/euro_time_heterogeneity.html",
             title = "Table 2: Euro Effects by Time Period")

cat("Regression tables saved\n\n")

#################################
# Section 7: Visualization
#################################

cat("\n========================================\n")
cat("SECTION 7: CREATE VISUALIZATIONS\n")
cat("========================================\n\n")

# Create treatment vs. control comparison
trade_trends <- gravity %>%
  group_by(year, BothEuro) %>%
  summarize(
    mean_trade = mean(trade_combined, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Group = ifelse(BothEuro == 1, "Both Euro", "Control")
  )

# Plot trends with darker colors and Times New Roman font
euro_plot <- ggplot(trade_trends, aes(x = year, y = log(mean_trade), 
                                      color = Group, group = Group)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 1999, linetype = "dashed", color = "gray30") +
  annotate("text", x = 1999, y = max(log(trade_trends$mean_trade), na.rm = TRUE), 
           label = "Euro Introduction", vjust = -0.5, size = 3.5, family = "serif") +
  scale_color_manual(values = c("Both Euro" = "#B22222",  
                                "Control" = "#00008B")) + 
  labs(
    title = "Average Bilateral Trade: Euro Members vs. Control",
    x = "Year",
    y = "Log Average Trade (Billions USD)",
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),  
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", family = "serif"),
    axis.title = element_text(size = 12, family = "serif"),
    axis.text = element_text(size = 10, family = "serif"),
    legend.title = element_text(size = 11, family = "serif"),
    legend.text = element_text(size = 10, family = "serif"),
    legend.position = "bottom"
  )

print(euro_plot)

ggsave("~/Desktop/euro_trade_trends.png", 
       plot = euro_plot, 
       width = 10, 
       height = 6, 
       dpi = 300)

cat("Visualization saved\n\n")

#################################
# 7B: Clean control group comparison
#################################

cat("\n========================================\n")
cat("SECTION 7B: CLEAN CONTROL GROUP (NEVER-EURO)\n")
cat("========================================\n\n")

# Define "never Euro" countries
# These are countries that never adopted the Euro during 1995-2020
never_euro_exp <- gravity %>%
  group_by(exporter_iso3) %>%
  summarize(ever_euro = max(euro_exporter, na.rm = TRUE), .groups = "drop") %>%
  filter(ever_euro == 0) %>%
  pull(exporter_iso3)

never_euro_imp <- gravity %>%
  group_by(importer_iso3) %>%
  summarize(ever_euro = max(euro_importer, na.rm = TRUE), .groups = "drop") %>%
  filter(ever_euro == 0) %>%
  pull(importer_iso3)

cat("Number of never-Euro countries (exporters):", length(never_euro_exp), "\n")
cat("Number of never-Euro countries (importers):", length(never_euro_imp), "\n\n")

# Create clean control group trends
# Treatment: Both in Euro
# Control: Both never adopted Euro (excludes future joiners like Greece, Slovenia, etc)
trade_trends_clean <- gravity %>%
  mutate(
    Group = case_when(
      BothEuro == 1 ~ "Both Euro",
      exporter_iso3 %in% never_euro_exp & importer_iso3 %in% never_euro_imp ~ "Never Euro",
      TRUE ~ "Excluded (Future Joiners)"
    )
  ) %>%
  filter(Group != "Excluded (Future Joiners)") %>%
  group_by(year, Group) %>%
  summarize(mean_trade = mean(trade_combined, na.rm = TRUE), .groups = "drop")

cat("Clean control group sample:\n")
print(table(trade_trends_clean$Group))
cat("\n")

# Plot clean comparison
euro_plot_clean <- ggplot(trade_trends_clean, aes(x = year, y = log(mean_trade), 
                                                  color = Group, group = Group)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 1999, linetype = "dashed", color = "gray30") +
  annotate("text", x = 1999, y = max(log(trade_trends_clean$mean_trade), na.rm = TRUE), 
           label = "Euro Introduction", vjust = -0.5, size = 3.5, family = "serif") +
  scale_color_manual(values = c("Both Euro" = "#B22222",     
                                "Never Euro" = "#00008B")) + 
  labs(
    title = "Average Bilateral Trade: Euro Pairs vs. Never-Euro Pairs",
    subtitle = "Control group excludes future Euro adopters (Greece, Slovenia, etc.)",
    x = "Year",
    y = "Log Average Trade (Billions USD)",
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", family = "serif"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, family = "serif"),
    axis.title = element_text(size = 12, family = "serif"),
    axis.text = element_text(size = 10, family = "serif"),
    legend.title = element_text(size = 11, family = "serif"),
    legend.text = element_text(size = 10, family = "serif"),
    legend.position = "bottom"
  )

print(euro_plot_clean)

ggsave("~/Desktop/euro_trade_trends_clean.png", 
       plot = euro_plot_clean, 
       width = 10, 
       height = 6, 
       dpi = 300)

cat("Clean control group visualization saved\n\n")

#################################
# Section 8: Robustness checks
#################################

cat("\n========================================\n")
cat("SECTION 8: ROBUSTNESS CHECKS\n")
cat("========================================\n\n")

#################################
# 8a: Exclude Greece
#################################

cat("Running regressions excluding Greece pairs\n")

# Create dataset without Greece
gravity_no_greece <- gravity %>%
  filter(exporter_iso3 != "GRC" & importer_iso3 != "GRC")

cat("Observations after excluding Greece:", nrow(gravity_no_greece), "\n\n")

# OLS Time Heterogeneity (No Greece)
ols_time_no_greece <- feols(log_trade ~ BothEuro_Early + BothEuro_Late + RTA | 
                              imp_year + exp_year + imp_exp_pair,
                            data = gravity_no_greece,
                            cluster = ~imp_exp_pair)

# PPML Time Heterogeneity (No Greece)
ppml_time_no_greece <- fepois(trade_ppml ~ BothEuro_Early + BothEuro_Late + RTA | 
                                imp_year + exp_year + imp_exp_pair,
                              data = gravity_no_greece,
                              cluster = ~imp_exp_pair)

cat("\n=== RESULTS EXCLUDING GREECE ===\n")
summary(ols_time_no_greece)
summary(ppml_time_no_greece)

# Rose Specification for Comparison
ols_rose <- feols(log_trade ~ BothEuro + RTA + 
                    log_gdp_o + log_gdp_d + 
                    log_dist + contig + comlang_off |
                    importer_iso3 + exporter_iso3 + year,
                  data = gravity,
                  cluster = ~imp_exp_pair)

summary(ols_rose)

# Save comparison table: Rose vs. Baseline
rose_comparison <- list(
  "Rose Spec (OLS)" = ols_rose,
  "Our Spec (OLS)"  = ols_baseline,
  "Our Spec (PPML)" = ppml_baseline
)

modelsummary(rose_comparison,
             stars = TRUE,
             gof_omit = "AIC|BIC|RMSE|Std.Errors",
             output = "~/Desktop/euro_rose_comparison.html",
             title = "Table: Rose (2000) vs. Saturated Specification")

#################################
# 8b: Founders vs joiners x time
#################################

cat("\n\nCreating Founders × Joiners × Time interactions\n")

# Create the four interaction terms
gravity <- gravity %>%
  mutate(
    BothFounders_Early = BothFounders * Early,
    BothFounders_Late = BothFounders * Late,
    AtLeastOneJoiner_Early = AtLeastOneJoiner * Early,
    AtLeastOneJoiner_Late = AtLeastOneJoiner * Late
  )

cat("Checking distribution of interactions:\n")
cat("BothFounders_Early:", sum(gravity$BothFounders_Early, na.rm = TRUE), "\n")
cat("BothFounders_Late:", sum(gravity$BothFounders_Late, na.rm = TRUE), "\n")
cat("AtLeastOneJoiner_Early:", sum(gravity$AtLeastOneJoiner_Early, na.rm = TRUE), "\n")
cat("AtLeastOneJoiner_Late:", sum(gravity$AtLeastOneJoiner_Late, na.rm = TRUE), "\n\n")

# OLS Founders/Joiners Decomposition
ols_founders_joiners <- feols(log_trade ~ BothFounders_Early + BothFounders_Late + 
                                AtLeastOneJoiner_Early + AtLeastOneJoiner_Late + RTA | 
                                imp_year + exp_year + imp_exp_pair,
                              data = gravity,
                              cluster = ~imp_exp_pair)

# PPML Founders/Joiners Decomposition
ppml_founders_joiners <- fepois(trade_ppml ~ BothFounders_Early + BothFounders_Late + 
                                  AtLeastOneJoiner_Early + AtLeastOneJoiner_Late + RTA | 
                                  imp_year + exp_year + imp_exp_pair,
                                data = gravity,
                                cluster = ~imp_exp_pair)

cat("\n=== FOUNDERS vs JOINERS × TIME ===\n")
summary(ols_founders_joiners)
summary(ppml_founders_joiners)

#################################
# 8c: Create tables
#################################

cat("\n\nCreating robustness tables\n")

# Table 3: Excluding Greece
no_greece_models <- list(
  "OLS" = ols_time_no_greece,
  "PPML" = ppml_time_no_greece
)

modelsummary(no_greece_models,
             stars = TRUE,
             gof_omit = "AIC|BIC|RMSE|Std.Errors",
             output = "~/Desktop/euro_no_greece_table.html",
             title = "Table 3: Time Heterogeneity (Excluding Greece)")

# Table 4: Founders vs Joiners
founders_joiners_models <- list(
  "OLS" = ols_founders_joiners,
  "PPML" = ppml_founders_joiners
)

modelsummary(founders_joiners_models,
             stars = TRUE,
             gof_omit = "AIC|BIC|RMSE|Std.Errors",
             output = "~/Desktop/euro_founders_joiners_table.html",
             title = "Table 4: Founders vs. Joiners by Time Period")

# Table 5: Side-by-side comparison (Original vs No Greece vs Founders/Joiners)
all_robustness_models <- list(
  "OLS: Original" = ols_time,
  "OLS: No Greece" = ols_time_no_greece,
  "OLS: Decomposed" = ols_founders_joiners,
  "PPML: Original" = ppml_time,
  "PPML: No Greece" = ppml_time_no_greece,
  "PPML: Decomposed" = ppml_founders_joiners
)

modelsummary(all_robustness_models,
             stars = TRUE,
             gof_omit = "AIC|BIC|RMSE|Std.Errors",
             output = "~/Desktop/euro_robustness_comparison.html",
             title = "Table 5: Robustness Checks - Full Comparison")

cat("Robustness tables saved to Desktop\n\n")

#################################
# 8d: Summary stats for robustness
#################################

cat("Summary of robustness findings:\n")
cat("================================\n\n")

cat("ORIGINAL TIME HETEROGENEITY:\n")
cat("OLS BothEuro_Early:", round(coef(ols_time)["BothEuro_Early"], 3), 
    "(SE:", round(se(ols_time)["BothEuro_Early"], 3), ")\n")
cat("OLS BothEuro_Late:", round(coef(ols_time)["BothEuro_Late"], 3), 
    "(SE:", round(se(ols_time)["BothEuro_Late"], 3), ")\n\n")

cat("EXCLUDING GREECE:\n")
cat("OLS BothEuro_Early:", round(coef(ols_time_no_greece)["BothEuro_Early"], 3), 
    "(SE:", round(se(ols_time_no_greece)["BothEuro_Early"], 3), ")\n")
cat("OLS BothEuro_Late:", round(coef(ols_time_no_greece)["BothEuro_Late"], 3), 
    "(SE:", round(se(ols_time_no_greece)["BothEuro_Late"], 3), ")\n\n")

cat("FOUNDERS vs JOINERS:\n")
cat("OLS BothFounders_Early:", round(coef(ols_founders_joiners)["BothFounders_Early"], 3), 
    "(SE:", round(se(ols_founders_joiners)["BothFounders_Early"], 3), ")\n")
cat("OLS BothFounders_Late:", round(coef(ols_founders_joiners)["BothFounders_Late"], 3), 
    "(SE:", round(se(ols_founders_joiners)["BothFounders_Late"], 3), ")\n")
cat("OLS AtLeastOneJoiner_Early:", round(coef(ols_founders_joiners)["AtLeastOneJoiner_Early"], 3), 
    "(SE:", round(se(ols_founders_joiners)["AtLeastOneJoiner_Early"], 3), ")\n")
cat("OLS AtLeastOneJoiner_Late:", round(coef(ols_founders_joiners)["AtLeastOneJoiner_Late"], 3), 
    "(SE:", round(se(ols_founders_joiners)["AtLeastOneJoiner_Late"], 3), ")\n\n")

cat("Robustness checks complete\n")

#################################
# FIGURE 2: Coefficient Plot — Table 4 (Founders vs. Joiners × Time)
# Add after Section 8d
#################################

# Pull coefficients and standard errors from OLS model
coef_plot_data <- data.frame(
  term = c("BothFounders_Early", "BothFounders_Late",
           "AtLeastOneJoiner_Early", "AtLeastOneJoiner_Late"),
  estimate = c(
    coef(ols_founders_joiners)["BothFounders_Early"],
    coef(ols_founders_joiners)["BothFounders_Late"],
    coef(ols_founders_joiners)["AtLeastOneJoiner_Early"],
    coef(ols_founders_joiners)["AtLeastOneJoiner_Late"]
  ),
  se = c(
    se(ols_founders_joiners)["BothFounders_Early"],
    se(ols_founders_joiners)["BothFounders_Late"],
    se(ols_founders_joiners)["AtLeastOneJoiner_Early"],
    se(ols_founders_joiners)["AtLeastOneJoiner_Late"]
  )
) %>%
  mutate(
    ci_low  = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se,
    # Readable labels for x-axis
    label = factor(term,
                   levels = c("BothFounders_Early", "BothFounders_Late",
                              "AtLeastOneJoiner_Early", "AtLeastOneJoiner_Late"),
                   labels = c("Founders\n(2000–2009)", "Founders\n(2010–2020)",
                              "Joiners\n(2000–2009)", "Joiners\n(2010–2020)")),
    # Color by cohort
    cohort = factor(c("Founders", "Founders", "Joiners", "Joiners"),
                    levels = c("Founders", "Joiners"))
  )

fig2_coef_plot <- ggplot(coef_plot_data, aes(x = label, y = estimate, color = cohort)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, linewidth = 1) +
  scale_color_manual(values = c("Founders" = "#00008B", "Joiners" = "#B22222")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(
    title = "Euro Trade Effects by Cohort and Time Period (OLS)",
    subtitle = "Coefficients with 95% confidence intervals. Baseline: non-Euro pairs.",
    x = NULL,
    y = "Estimated Effect on Log Bilateral Trade",
    color = "Cohort"
  ) +
  theme_minimal() +
  theme(
    text               = element_text(family = "serif"),
    plot.title         = element_text(hjust = 0.5, size = 14, face = "bold", family = "serif"),
    plot.subtitle      = element_text(hjust = 0.5, size = 10, family = "serif"),
    axis.title         = element_text(size = 12, family = "serif"),
    axis.text          = element_text(size = 11, family = "serif"),
    legend.title       = element_text(size = 11, family = "serif"),
    legend.text        = element_text(size = 10, family = "serif"),
    legend.position    = "bottom",
    panel.grid.major.x = element_blank()
  )

print(fig2_coef_plot)

ggsave("~/Desktop/euro_coef_plot_founders_joiners.png",
       plot   = fig2_coef_plot,
       width  = 10,
       height = 6,
       dpi    = 300)

cat("Figure 2 (coefficient plot) saved to Desktop\n\n")


#################################
# FIGURE 3: Year-by-Year Event Study
# Runs a separate OLS regression for each year's BothEuro × Year interaction
# to show how the Euro effect evolves over time
#################################

event_years <- c(1995:1997, 1999:2020)  # exclude 1998 (base)

# Create year dummies interacted with BothEuro
for (yr in event_years) {
  gravity[[paste0("BothEuro_yr", yr)]] <- as.integer(gravity$BothEuro == 1 & gravity$year == yr)
}

# Build formula dynamically
event_vars <- paste0("BothEuro_yr", event_years, collapse = " + ")
event_formula <- as.formula(
  paste("log_trade ~", event_vars, "+ RTA | imp_year + exp_year + imp_exp_pair")
)

cat("Running event study regression...\n")
ols_event <- feols(event_formula,
                   data    = gravity,
                   cluster = ~imp_exp_pair)

# Extract coefficients
event_coefs <- data.frame(
  year     = event_years,
  estimate = coef(ols_event)[paste0("BothEuro_yr", event_years)],
  se       = se(ols_event)[paste0("BothEuro_yr", event_years)]
) %>%
  # Add the base year 1998 = 0
  bind_rows(data.frame(year = 1998, estimate = 0, se = 0)) %>%
  arrange(year) %>%
  mutate(
    ci_low  = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se
  )

fig3_event_study <- ggplot(event_coefs, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_vline(xintercept = 1998.5, linetype = "dotted", color = "gray40", linewidth = 0.8) +
  annotate("text", x = 1999, y = max(event_coefs$ci_high, na.rm = TRUE) * 0.95,
           label = "Euro\nIntroduction", hjust = 0, size = 3.2, family = "serif", color = "gray30") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#B22222", alpha = 0.15) +
  geom_line(color = "#B22222", linewidth = 1.1) +
  geom_point(color = "#B22222", size = 2.5) +
  # Shade the debt crisis era
  annotate("rect", xmin = 2010, xmax = 2012, ymin = -Inf, ymax = Inf,
           fill = "gray70", alpha = 0.25) +
  annotate("text", x = 2011, y = min(event_coefs$ci_low, na.rm = TRUE) * 0.85,
           label = "Debt\nCrisis", hjust = 0.5, size = 3.0, family = "serif", color = "gray40") +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5)) +
  labs(
    title    = "Year-by-Year Euro Trade Effect (Event Study)",
    subtitle = "OLS coefficients on BothEuro × Year, relative to 1998. 95% CI shaded. All Euro pairs.",
    x        = "Year",
    y        = "Estimated Effect on Log Bilateral Trade"
  ) +
  theme_minimal() +
  theme(
    text          = element_text(family = "serif"),
    plot.title    = element_text(hjust = 0.5, size = 14, face = "bold", family = "serif"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, family = "serif"),
    axis.title    = element_text(size = 12, family = "serif"),
    axis.text     = element_text(size = 10, family = "serif")
  )

print(fig3_event_study)

ggsave("~/Desktop/euro_event_study.png",
       plot   = fig3_event_study,
       width  = 10,
       height = 6,
       dpi    = 300)

cat("Figure 3 (event study) saved to Desktop\n\n")