# Code written by Group 4
# Econometrics Project
# University of Texas at Dallas

# rm(list=ls())
# install.packages("stargazer")

# First set up our working directory
wd <- "C:/Users/Jeremy/OneDrive/Documents/UTD/BUAN 6312/Project/"

# Load required packages
library(AER)           # For USSeatBelts dataset
library(dplyr)         # Data manipulation
library(ggplot2)       # Plotting
library(car)           # 
library(stargazer)     # Regression tables
library(tidyverse)     # Data manipulation


# ------------------------------------------------------------------
# -------------------------------------------- Read in Data --------
# ------------------------------------------------------------------

# Load the dataset
data("USSeatBelts")
data_raw <- USSeatBelts


# ------------------------------------------------------------------
# ----------------------------------------- Manipulate Data --------
# ------------------------------------------------------------------


# Find list of states that had a secondary seat belt law
states_with_secondary <- data_raw %>%
  filter(enforce == "secondary") %>%
  distinct(state) %>%
  pull(state)

states_with_primary <- data_raw %>%
  filter(enforce == "primary") %>%
  distinct(state) %>%
  pull(state)


# Filter data into primary and secondary enforcement states
data_secondary_raw <- data_raw %>%
  filter(state %in% states_with_secondary) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  mutate(law_binary = ifelse(enforce == "no", 0, 1))

data_primary_raw <- data_raw %>%
  filter(state %in% states_with_primary) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  mutate(law_binary = ifelse(enforce == "no", 0, 1))


# Get year of first secondary enforcement per state
df_enforce_starts <- data_secondary_raw %>%
  filter(enforce == "secondary") %>%
  group_by(state) %>%
  summarize(first_law_year = min(year), .groups = "drop")

# Join to main data the relative year of policy
data_secondary <- data_secondary_raw %>%
  left_join(., df_enforce_starts, by = "state") %>%
  mutate(relative_year = year - first_law_year)  # Years since enforcement

# ─────────────────────────────────────────────────────────────
#   QUICK DESCRIPTIVE TABLE  ▸  Summary by enforcement category
# ─────────────────────────────────────────────────────────────
# Load required packages
library(modelsummary)
library(dplyr)

# Rename 'enforce' for clearer labeling
data_raw$enforcement <- data_raw$enforce

# Generate summary statistics grouped by enforcement type
datasummary_skim(
  data_raw %>%
    select(enforcement, fatalities, seatbelt, income),
  by = "enforcement",
  title = "Descriptive Statistics by Enforcement Type"
)


# ------------------------------------------------------------------
# ------------------------ Difference-in-Differences Plot ----------
# ------------------------------------------------------------------
# Purpose: Visualize trends in average fatalities per mile
#          for treated (secondary enforcement) vs control (no enforcement) states
#          to assess pre-trends and post-law divergence
# ------------------------------------------------------------------
data("USSeatBelts")
data_raw <- USSeatBelts

# Convert year from factor to numeric
data_raw$year <- as.numeric(as.character(data_raw$year))

# Create a treatment indicator
data_raw <- data_raw %>%
  mutate(
    treatment_group = ifelse(enforce == "secondary", 1, 0),
    group_label = case_when(
      enforce == "secondary" ~ "Treated (Secondary Enforcement)",
      enforce == "no" ~ "Control (No Law)",
      TRUE ~ NA_character_
    )
  )

# Identify first year of law in each treated state
df_law_start <- data_raw %>%
  filter(treatment_group == 1) %>%
  group_by(state) %>%
  summarize(first_law_year = min(year), .groups = "drop")

# Merge with main dataset to calculate relative year
data_did <- data_raw %>%
  left_join(df_law_start, by = "state") %>%
  mutate(
    relative_year = year - first_law_year,
    event_window = between(relative_year, -10, 10)
  ) %>%
  filter(group_label %in% c("Treated (Secondary Enforcement)", "Control (No Law)"))

# Calculate average fatalities by group and relative year
did_avg <- data_did %>%
  filter(event_window) %>%
  group_by(relative_year, group_label) %>%
  summarize(
    avg_fatal = mean(fatalities, na.rm = TRUE),
    se = sd(fatalities, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot DiD graph
ggplot(did_avg, aes(x = relative_year, y = avg_fatal, color = group_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = avg_fatal - 1.96 * se, ymax = avg_fatal + 1.96 * se),
                width = 0.3) +
  labs(
    title = "Difference-in-Differences: Secondary Enforcement vs. No Law",
    x = "Years Since Law Passed",
    y = "Avg. Fatalities per Mile",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    legend.position = "top"
  )




# ------------------------------------------------------------------
# --------------------------------------------- Data Charts --------
# ------------------------------------------------------------------

# Look at average fatalities before and after secondary laws
# make data to use for chart with averages
df_avg <- data_secondary %>%
  filter(between(relative_year, -10, 10)) %>%
  group_by(relative_year) %>%
  summarize(mean_fatalities = mean(fatalities, na.rm = TRUE),
            se = sd(fatalities, na.rm = TRUE) / sqrt(n()))

# create chart
ggplot(df_avg, aes(x = relative_year, y = mean_fatalities)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray20") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray30") +
  geom_point(color = "steelblue", size = 2) +
  geom_errorbar(aes(ymin = mean_fatalities - 1.96 * se,
                    ymax = mean_fatalities + 1.96 * se),
                color = "steelblue", width = 0.3) +
  labs(title = "Average Fatalities by Event Time (Secondary Laws)",
       x = "Years Since Law Passed", y = "Avg. Fatalities per Mile") +
  theme_minimal()


# ------------------------------------------------------------------
# --------------------------------- First Regression Models --------
# ------------------------------------------------------------------


# Naive OLS (no fixed effects)
model_secondary_naive <- lm(log(fatalities) ~ log(seatbelt), data = data_secondary)
summary(model_secondary_naive)

# FE Only: add state and year fixed effects as factor variables
model_secondary_FE <- lm(log(fatalities) ~ log(seatbelt) + factor(state) + factor(year), data = data_secondary)
summary(model_secondary_FE)

# FE + Controls
model_secondary_FE_controls <- lm(log(fatalities) ~ log(seatbelt) + log(income) + alcohol + speed65 + drinkage +
            factor(state) + factor(year), data = data_secondary)
summary(model_secondary_FE_controls)


# rename to make it easier to read stargazer function
lm1 <- model_secondary_naive
lm2 <- model_secondary_FE
lm3 <- model_secondary_FE_controls


# combine outputs of all three models
stargazer(lm1, lm2, lm3,
          type = "text",
          title = "Regression Results: Secondary Enforcement States",
          column.labels = c("Naive", "FE Only", "FE + Controls"),
          dep.var.labels = "log(Fatalities)",
          covariate.labels = c("Log Seatbelt Use", "Log Income", "Alcohol Law", 
                               "65 MPH", "MLDA"),
          omit = c("factor\\(state\\)", "factor\\(year\\)"),
          omit.stat = c("f", "ser", "adj.rsq"),
          add.lines = list(
            c("State FE", "No", "Yes", "Yes"),
            c("Year FE", "No", "Yes", "Yes")
          ),
          no.space = TRUE,
          digits = 3,
          star.cutoffs = c(0.1, 0.05, 0.01)
)


# ------------------------------------------------------------------
# ----------------------------- Instrument Variable / 2SLS  --------
# ------------------------------------------------------------------

# First stage regression: instrument seatbelt usage with secondary enforcement law_binary
first_stage <- lm(
  log(seatbelt) ~ law_binary + log(income) + alcohol + speed65 + drinkage +
    factor(state) + factor(year),
  data = data_secondary
)

# Create a clean version of the data actually used
first_stage_data <- model.frame(first_stage)

# ------------------ First-Stage Diagnostics ---------------------
# Print F-statistic from first stage to assess instrument strength
summary_first_stage <- summary(first_stage)
f_stat <- summary_first_stage$fstatistic
cat("First-stage F-statistic (Secondary):", round(f_stat[1], 2), 
    "on", f_stat[2], "and", f_stat[3], "DF\n")


# Add predicted values (fitted values from first stage)
first_stage_data$seatbelt_hat <- predict(first_stage)

# Join predicted values back into main dataset
data_2SLS_secondary <- first_stage_data %>%
  dplyr::select(seatbelt_hat, `factor(state)`, `factor(year)`) %>%
  rename("state" = `factor(state)`, "year" = `factor(year)`) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  left_join(., data_secondary, by = join_by(state, year))

# Second stage regression using predicted seatbelt usage
second_stage <- lm(
  log(fatalities) ~ seatbelt_hat + log(income) + alcohol + speed65 + drinkage +
    factor(state) + factor(year),
  data = data_2SLS_secondary
)
summary(second_stage)





# ------------------------------------------------------------------
# -------------------------- PRIMARY LAW ANALYSIS ------------------
# ------------------------------------------------------------------

# Identify primary-law states
states_with_primary <- data_raw %>%
  filter(enforce == "primary") %>%
  distinct(state) %>%
  pull(state)

# Filter and preprocess data
data_primary_raw <- data_raw %>%
  filter(state %in% states_with_primary) %>%
  mutate(
    year = as.numeric(as.character(year)),
    law_binary = ifelse(enforce == "no", 0, 1)
  )

# Get first year of primary law
df_enforce_primary <- data_primary_raw %>%
  filter(enforce == "primary") %>%
  group_by(state) %>%
  summarize(first_law_year = min(year), .groups = "drop")

# Merge first_law_year and compute relative time
data_primary <- data_primary_raw %>%
  left_join(., df_enforce_primary, by = "state") %>%
  mutate(relative_year = year - first_law_year)


# ─────────────────────────────────────────────────────────────────────
#        PLOT AVERAGE FATALITIES AROUND PRIMARY ENFORCEMENT LAWS
# ─────────────────────────────────────────────────────────────────────
# This section:
# - Filters data from 10 years before to 10 years after primary law enactment
# - Aggregates average fatalities and standard errors by relative year
# - Plots the result with 95% confidence intervals
# ─────────────────────────────────────────────────────────────────────

# Create summary table for plotting
df_avg_primary <- data_primary %>%
  filter(between(relative_year, -10, 10)) %>%
  group_by(relative_year) %>%
  summarize(
    mean_fatalities = mean(fatalities, na.rm = TRUE),
    se = sd(fatalities, na.rm = TRUE) / sqrt(n())
  )

# Plot average fatalities with confidence intervals
ggplot(df_avg_primary, aes(x = relative_year, y = mean_fatalities)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray20") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray30") +
  geom_point(color = "darkgreen", size = 2) +
  geom_errorbar(aes(
    ymin = mean_fatalities - 1.96 * se,
    ymax = mean_fatalities + 1.96 * se
  ),
  color = "darkgreen", width = 0.3) +
  labs(
    title = "Average Fatalities by Event Time (Primary Laws)",
    x = "Years Since Law Passed",
    y = "Avg. Fatalities per Mile"
  ) +
  theme_minimal()




# ─────────────────────────────────────────────────────────────────────
#          REGRESSION ANALYSIS: PRIMARY ENFORCEMENT STATES
# ─────────────────────────────────────────────────────────────────────
# This section runs 3 models on the primary dataset:
# (1) Naive OLS
# (2) Fixed Effects with State & Year
# (3) FE + Controls (income, alcohol, speed limit, drinking age)
# ─────────────────────────────────────────────────────────────────────

# Naive OLS (no fixed effects)
model_primary_naive <- lm(log(fatalities) ~ log(seatbelt), data = data_primary)

# Fixed Effects only
model_primary_FE <- lm(log(fatalities) ~ log(seatbelt) + factor(state) + factor(year),
                       data = data_primary)

# Fixed Effects + Controls
model_primary_FE_controls <- lm(log(fatalities) ~ log(seatbelt) + log(income) +
                                  alcohol + speed65 + drinkage +
                                  factor(state) + factor(year),
                                data = data_primary)



# ─────────────────────────────────────────────────────────────────────
#         STARGAZER TABLE: PRIMARY ENFORCEMENT REGRESSION MODELS
# ─────────────────────────────────────────────────────────────────────
# This formats and outputs a comparison table across:
# (1) Naive OLS
# (2) Fixed Effects
# (3) FE + Controls
# ─────────────────────────────────────────────────────────────────────

# Display all three models side by side
stargazer(model_primary_naive, model_primary_FE, model_primary_FE_controls,
          type = "text",
          title = "Regression Results: Primary Enforcement States",
          column.labels = c("Naive", "FE Only", "FE + Controls"),
          dep.var.labels = "log(Fatalities)",
          covariate.labels = c("Log Seatbelt Use", "Log Income", "Alcohol Law", 
                               "65 MPH", "MLDA"),
          omit = c("factor\\(state\\)", "factor\\(year\\)"),
          omit.stat = c("f", "ser", "adj.rsq"),
          add.lines = list(
            c("State FE", "No", "Yes", "Yes"),
            c("Year FE", "No", "Yes", "Yes")
          ),
          no.space = TRUE,
          digits = 3,
          star.cutoffs = c(0.1, 0.05, 0.01))



# ─────────────────────────────────────────────────────────────────────
#           2SLS ANALYSIS: PRIMARY ENFORCEMENT AS INSTRUMENT
# ─────────────────────────────────────────────────────────────────────
# ─────────────────────────────────────────────────────────────────────
#  Instrument Strength Check: First-stage F-stat for Primary Enforcement
# ─────────────────────────────────────────────────────────────────────

# Add a row index to match filtered rows later
data_primary$row_id <- 1:nrow(data_primary)

# First-stage regression
first_stage_primary <- lm(
  log(seatbelt) ~ law_binary + log(income) + alcohol + speed65 + drinkage +
    factor(state) + factor(year),
  data = data_primary
)

# First-stage diagnostics: Instrument strength check
summary_first_stage_primary <- summary(first_stage_primary)
f_stat_primary <- summary_first_stage_primary$fstatistic
cat("First-stage F-statistic (Primary):", round(f_stat_primary[1], 2), 
    "on", f_stat_primary[2], "and", f_stat_primary[3], "DF\n")

summary_first_stage_primary <- summary(first_stage_primary)
f_stat_primary <- summary_first_stage_primary$fstatistic
cat("First-stage F-statistic (Primary):", round(f_stat_primary[1], 2), 
    "on", f_stat_primary[2], "and", f_stat_primary[3], "DF\n")


# Extract complete-case data used in first stage
first_stage_data_primary <- model.frame(first_stage_primary)

# Add predicted seatbelt use
first_stage_data_primary$seatbelt_hat <- predict(first_stage_primary)

# Recover row numbers of used observations
row_ids <- as.numeric(rownames(first_stage_data_primary))

# Rebuild dataset for second stage using same rows as first stage
data_2SLS_primary <- data_primary[row_ids, ] %>%
  mutate(seatbelt_hat = first_stage_data_primary$seatbelt_hat)

# Second-stage regression: fatalities ~ predicted seatbelt use
second_stage_primary <- lm(
  log(fatalities) ~ seatbelt_hat + log(income) + alcohol + speed65 + drinkage +
    factor(state) + factor(year),
  data = data_2SLS_primary
)

# Display output
summary(second_stage_primary)


# ─────────────────────────────────────────────────────────────────────
#        STARGAZER COMPARISON: 2SLS RESULTS (PRIMARY VS SECONDARY)
# ─────────────────────────────────────────────────────────────────────

stargazer(second_stage, second_stage_primary,
          type = "text",
          title = "2SLS Regression Results: Secondary vs. Primary Enforcement",
          column.labels = c("Secondary Laws", "Primary Laws"),
          dep.var.labels = "log(Fatalities)",
          covariate.labels = c("Predicted Seatbelt Use", "Log Income", "Alcohol Law", 
                               "65 MPH", "MLDA"),
          omit = c("factor\\(state\\)", "factor\\(year\\)"),
          omit.stat = c("f", "ser", "adj.rsq"),
          add.lines = list(
            c("State FE", "Yes", "Yes"),
            c("Year FE", "Yes", "Yes")
          ),
          no.space = TRUE,
          digits = 3,
          star.cutoffs = c(0.1, 0.05, 0.01))




# ─────────────────────────────────────────────────────────────────────
#       2SLS Coefficient Comparison: Predicted Seatbelt Use Effect
# ─────────────────────────────────────────────────────────────────────

# Load necessary package
library(ggplot2)

# Create a dataframe of coefficient estimates and standard errors
coeffs <- data.frame(
  Model = c("Secondary Enforcement", "Primary Enforcement"),
  Estimate = c(-0.099, -0.154),
  Std_Error = c(0.055, 0.194)
)

# Plot the bar chart with error bars
ggplot(coeffs, aes(x = Model, y = Estimate, fill = Model)) +
  geom_bar(stat = "identity", width = 0.6, color = "black", show.legend = FALSE) +
  geom_errorbar(aes(ymin = Estimate - 1.96 * Std_Error, ymax = Estimate + 1.96 * Std_Error),
                width = 0.2, linewidth = 1.0) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  scale_fill_manual(values = c("darkgreen", "steelblue")) +
  labs(
    title = "2SLS Coefficient Comparison:\nSecondary vs. Primary Enforcement",
    y = "Effect of Predicted Seatbelt Use on log(Fatalities)",
    x = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 13, hjust = 0.5),
    axis.text.x = element_text(size = 12),
    plot.margin = margin(t = 10, r = 10, b = 20, l = 10)
  )



# ─────────────────────────────────────────────────────────────
#       DiD Graph: Primary Enforcement vs. No Law (Control)
# ─────────────────────────────────────────────────────────────

# Create treatment indicator for primary laws
data_raw <- data_raw %>%
  mutate(
    primary_group = ifelse(enforce == "primary", 1, 0),
    group_label_primary = case_when(
      enforce == "primary" ~ "Treated (Primary Enforcement)",
      enforce == "no" ~ "Control (No Law)",
      TRUE ~ NA_character_
    )
  )

# Identify first year of law in each treated state
df_primary_start <- data_raw %>%
  filter(primary_group == 1) %>%
  group_by(state) %>%
  summarize(first_law_year = min(year), .groups = "drop")

# Merge to get relative year
data_did_primary <- data_raw %>%
  left_join(df_primary_start, by = "state") %>%
  mutate(
    relative_year = year - first_law_year,
    event_window = between(relative_year, -10, 10)
  ) %>%
  filter(group_label_primary %in% c("Treated (Primary Enforcement)", "Control (No Law)"))

# Compute group averages
did_avg_primary <- data_did_primary %>%
  filter(event_window) %>%
  group_by(relative_year, group_label_primary) %>%
  summarize(
    avg_fatal = mean(fatalities, na.rm = TRUE),
    se = sd(fatalities, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Plot
ggplot(did_avg_primary, aes(x = relative_year, y = avg_fatal, color = group_label_primary)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = avg_fatal - 1.96 * se, ymax = avg_fatal + 1.96 * se),
                width = 0.3) +
  labs(
    title = "Difference-in-Differences: Primary Enforcement vs. No Law",
    x = "Years Since Law Passed",
    y = "Avg. Fatalities per Mile",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    legend.position = "top"
  )


# ─────────────────────────────────────────────────────────────
#                Histogram of Seatbelt Usage Rates
# ─────────────────────────────────────────────────────────────

library(ggplot2)

ggplot(data_raw, aes(x = seatbelt)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black", alpha = 0.8) +
  labs(
    title = "Distribution of Seatbelt Usage Rates (1984–1997)",
    x = "Seatbelt Usage Rate",
    y = "Number of State-Year Observations"
  ) +
  theme_minimal(base_size = 13)

#------------------------------------------------------------
#                             TABLES
#-------------------------------------------------------------

stargazer(lm1, lm2, lm3, second_stage,
          type = "text",
          title = "Regression Results: Secondary Enforcement States",
          column.labels = c("Naive", "FE", "FE + Controls", "2SLS"),
          dep.var.labels = "log(Fatalities)",
          omit = c("factor\\(state\\)", "factor\\(year\\)"),
          omit.stat = c("f", "ser", "adj.rsq"),
          add.lines = list(
            c("State FE", "No", "Yes", "Yes", "Yes"),
            c("Year FE", "No", "Yes", "Yes", "Yes")
          ),
          no.space = TRUE,
          digits = 3,
          star.cutoffs = c(0.1, 0.05, 0.01))


# Rename for stargazer
lm1_primary <- model_primary_naive
lm2_primary <- model_primary_FE
lm3_primary <- model_primary_FE_controls
lm4_primary <- second_stage_primary  # 2SLS model

# Stargazer output for all 4 models
stargazer(lm1_primary, lm2_primary, lm3_primary, lm4_primary,
          type = "text",
          title = "Regression Results: Primary Enforcement States",
          column.labels = c("Naive", "FE", "FE + Controls", "2SLS"),
          dep.var.labels = "log(Fatalities)",
          omit = c("factor\\(state\\)", "factor\\(year\\)"),
          omit.stat = c("f", "ser", "adj.rsq"),
          add.lines = list(
            c("State FE", "No", "Yes", "Yes", "Yes"),
            c("Year FE", "No", "Yes", "Yes", "Yes")
          ),
          no.space = TRUE,
          digits = 3,
          star.cutoffs = c(0.1, 0.05, 0.01))





