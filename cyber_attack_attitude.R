df <- read.csv("WVS7_with_espionage_dates_treated_cat (1).csv")
library(dplyr)
library(lubridate)

# Convert interview_date to Date format and extract year-month
df$interview_date <- as.Date(df$interview_date)
df$year_month <- floor_date(df$interview_date, "month")

# Aggregate to country-month level
country_month_df <- df %>%
  group_by(country_alpha3, year_month) %>%
  summarise(
    Q65_mean = mean(Q65_Armed.Forces, na.rm = TRUE),
    Q69_mean = mean(Q69_Police, na.rm = TRUE),
    Q70_mean = mean(Q70_Justice.System.Courts, na.rm = TRUE),
    Q71_mean = mean(Q71_Government..in.your.nation.s.capital., na.rm = TRUE),
    Q72_mean = mean(Q72_Political.Parties, na.rm = TRUE),
    Q73_mean = mean(Q73_Parliament, na.rm = TRUE),
    Q74_mean = mean(Q74_Civil.Services, na.rm = TRUE),
    Q76_mean = mean(Q76_Elections, na.rm = TRUE),
    treated_6m = as.integer(sum(treated_6m, na.rm = TRUE) > 0),
    n_respondents = n(),
    .groups = 'drop'
  )

print(head(country_month_df, 20))

# Event Study Analysis using fixest
# Install fixest if not already installed
if (!require("fixest", quietly = TRUE)) {
  install.packages("fixest")
}
library(fixest)

# Create event time: time relative to first treatment
country_month_df <- country_month_df %>%
  group_by(country_alpha3) %>%
  mutate(
    first_treatment = min(year_month[treated_6m == 1], na.rm = TRUE),
    first_treatment = ifelse(is.infinite(first_treatment), NA, first_treatment),
    event_time = as.numeric(difftime(year_month, as.Date(first_treatment), units = "days")) / 30.44  # Convert to months
  ) %>%
  ungroup()

# Run TWFE event study model with Q65 as outcome
# i(event_time, ref = -1) creates event time dummies, using -1 as reference period
# | country_alpha3 + year_month adds two-way fixed effects
event_study_model <- feols(
  Q65_mean ~ i(event_time, ref = -1) | country_alpha3 + year_month,
  data = country_month_df,
  cluster = ~country_alpha3  # Cluster standard errors at country level
)

# Display results
summary(event_study_model)

# Plot event study coefficients
iplot(event_study_model, 
      main = "Event Study: Effect on Trust in Armed Forces (Q65)",
      xlab = "Months Relative to Cyber Espionage Event",
      ylab = "Coefficient Estimate")

