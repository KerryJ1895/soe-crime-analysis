# Division-Date Panel Analysis - SOE ONLY
# Simplified version focusing only on State of Emergency interventions

# Load required libraries
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(stringr)
library(fixest)

# File paths
crime_file <- "C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/Crime file 2016-2020.xlsx"
soe_file <- "C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/ZOSO_and_SOE.xlsx"

# Function to create SOE-only panel
create_soe_only_panel <- function() {
  cat("=== CREATING SOE-ONLY DIVISION-DATE PANEL ===\n\n")
  
  # Load crime data
  cat("Loading crime data...\n")
  crime_df <- read_excel(crime_file, sheet = "Sheet (2)")
  
  # Load SOE data only
  cat("Loading SOE data...\n")
  soe_df <- read_excel(soe_file, sheet = "SOE")
  
  # Divisions that have SOE interventions
  soe_divisions <- unique(soe_df$Division)
  cat("Divisions with SOE:", paste(soe_divisions, collapse = ", "), "\n")
  
  # Include ALL divisions for comparison (treated + control)
  all_divisions <- unique(crime_df$DIVISION)
  cat("All divisions in crime data:", length(all_divisions), "\n")
  
  # Create date sequence
  date_seq <- seq(as.Date("2016-01-01"), as.Date("2020-12-31"), by = "day")
  
  # Create panel structure
  panel_df <- expand_grid(
    division = all_divisions,
    date = date_seq
  ) %>%
    mutate(
      year = year(date),
      month = month(date),
      quarter = quarter(date)
    )
  
  cat("Panel skeleton created:", nrow(panel_df), "division-date observations\n")
  
  # Aggregate crime data by division and date
  crime_daily <- crime_df %>%
    mutate(date = as.Date(DATE_COMMITTED)) %>%
    group_by(DIVISION, date) %>%
    summarise(
      crime_total = n(),
      robbery = sum(CATEGORY == "Robbery"),
      shooting = sum(CATEGORY == "Shooting"),
      murder = sum(CATEGORY == "Murder"),
      .groups = "drop"
    ) %>%
    rename(division = DIVISION)
  
  # Merge crime data with panel
  panel_df <- panel_df %>%
    left_join(crime_daily, by = c("division", "date")) %>%
    mutate(across(crime_total:murder, ~replace_na(., 0)))
  
  # Add SOE treatment indicators
  panel_df <- panel_df %>%
    mutate(
      treated = as.numeric(division %in% soe_divisions),
      post = 0,
      soe_active = 0
    )
  
  # Add SOE periods
  for (i in 1:nrow(soe_df)) {
    div <- soe_df$Division[i]
    start_date <- as.Date(soe_df$SOE_Effective_Date[i])
    end_date <- if (is.na(soe_df$SOE_End_Date[i])) as.Date("2020-12-31") else as.Date(soe_df$SOE_End_Date[i])
    
    panel_df <- panel_df %>%
      mutate(
        post = case_when(
          division == div & date >= start_date ~ 1,
          TRUE ~ post
        ),
        soe_active = case_when(
          division == div & date >= start_date & date <= end_date ~ 1,
          TRUE ~ soe_active
        )
      )
  }
  
  cat("SOE treatment indicators added\n")
  
  # Show panel structure
  cat("\n=== PANEL STRUCTURE PREVIEW ===\n")
  sample_panel <- panel_df %>%
    filter(division %in% c("St. James", "Portland") & date >= "2018-01-15" & date <= "2018-01-20") %>%
    select(division, date, year, treated, post, soe_active, crime_total, robbery, shooting, murder)
  
  print(sample_panel)
  
  # Create monthly aggregation (main analysis dataset)
  monthly_panel <- panel_df %>%
    mutate(year_month = floor_date(date, "month")) %>%
    group_by(division, year_month, year, month, quarter, treated) %>%
    summarise(
      crime_total = sum(crime_total),
      robbery = sum(robbery),
      shooting = sum(shooting),
      murder = sum(murder),
      post = max(post),
      soe_active = max(soe_active),
      .groups = "drop"
    ) %>%
    arrange(division, year_month)
  
  cat("\n=== MONTHLY PANEL STRUCTURE ===\n")
  sample_monthly <- monthly_panel %>%
    filter(division %in% c("St. James", "Portland") & year == 2018 & month %in% 1:3) %>%
    select(division, year_month, year, treated, post, soe_active, crime_total, robbery)
  
  print(sample_monthly)
  
  # Summary statistics
  cat("\n=== TREATMENT SUMMARY ===\n")
  treatment_summary <- monthly_panel %>%
    group_by(treated, soe_active) %>%
    summarise(
      observations = n(),
      divisions = n_distinct(division),
      mean_crimes = mean(crime_total),
      .groups = "drop"
    ) %>%
    mutate(
      group = case_when(
        treated == 0 ~ "Control (Never Treated)",
        treated == 1 & soe_active == 0 ~ "Treatment (Pre-SOE)",
        treated == 1 & soe_active == 1 ~ "Treatment (During SOE)"
      )
    )
  
  print(treatment_summary)
  
  return(list(
    daily_panel = panel_df,
    monthly_panel = monthly_panel
  ))
}

# Function to run SOE analysis
run_soe_analysis <- function(monthly_panel) {
  cat("\n=== SOE DIFFERENCE-IN-DIFFERENCES ANALYSIS ===\n")
  
  # Basic DiD model: Y = α + β1*treated + β2*post + β3*(treated*post) + ε
  # But we use soe_active instead of simple post since SOEs have different timing
  
  # Model 1: Basic SOE effect
  model1 <- feols(crime_total ~ treated * soe_active | division + year_month, 
                  data = monthly_panel,
                  vcov = "cluster")
  
  cat("\nModel 1: Total Crimes - SOE Treatment Effect\n")
  print(summary(model1))
  
  # Model 2: Robbery
  model2 <- feols(robbery ~ treated * soe_active | division + year_month, 
                  data = monthly_panel,
                  vcov = "cluster")
  
  cat("\nModel 2: Robbery - SOE Treatment Effect\n")
  print(summary(model2))
  
  # Model 3: Shooting
  model3 <- feols(shooting ~ treated * soe_active | division + year_month, 
                  data = monthly_panel,
                  vcov = "cluster")
  
  cat("\nModel 3: Shooting - SOE Treatment Effect\n")
  print(summary(model3))
  
  # Model 4: Murder  
  model4 <- feols(murder ~ treated * soe_active | division + year_month, 
                  data = monthly_panel,
                  vcov = "cluster")
  
  cat("\nModel 4: Murder - SOE Treatment Effect\n")
  print(summary(model4))
  
  return(list(
    total_crimes = model1,
    robbery = model2,
    shooting = model3,
    murder = model4
  ))
}

# Function to create SOE visualizations
create_soe_plots <- function(monthly_panel) {
  cat("\n=== CREATING SOE VISUALIZATIONS ===\n")
  
  # Plot 1: Time series by treatment status
  plot_data1 <- monthly_panel %>%
    mutate(
      group = case_when(
        treated == 0 ~ "Control",
        treated == 1 & soe_active == 0 ~ "Pre-SOE", 
        treated == 1 & soe_active == 1 ~ "During SOE"
      )
    ) %>%
    group_by(year_month, group) %>%
    summarise(mean_crimes = mean(crime_total), .groups = "drop")
  
  p1 <- ggplot(plot_data1, aes(x = year_month, y = mean_crimes, color = group)) +
    geom_line(linewidth = 1) +
    geom_point() +
    labs(
      title = "Average Monthly Crimes: Control vs SOE Treatment",
      x = "Date",
      y = "Average Monthly Crimes",
      color = "Group"
    ) +
    theme_minimal() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")
  
  print(p1)
  
  # Plot 2: SOE Timeline by Division
  soe_timeline <- monthly_panel %>%
    filter(soe_active == 1) %>%
    group_by(division) %>%
    summarise(
      soe_start = min(year_month),
      soe_end = max(year_month),
      .groups = "drop"
    )
  
  p2 <- ggplot(soe_timeline, aes(y = reorder(division, soe_start))) +
    geom_segment(aes(x = soe_start, xend = soe_end), 
                 size = 4, color = "red", alpha = 0.7) +
    labs(
      title = "SOE Implementation Timeline by Division",
      x = "Date", 
      y = "Division"
    ) +
    theme_minimal() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year")
  
  print(p2)
  
  return(list(p1 = p1, p2 = p2))
}

# Main execution
cat("JAMAICA SOE ANALYSIS - DIVISION-DATE PANEL\n")
cat("=" , rep("=", 50), "\n")

# Create the panel
panel_results <- create_soe_only_panel()
daily_panel <- panel_results$daily_panel
monthly_panel <- panel_results$monthly_panel

# Run analysis
regression_results <- run_soe_analysis(monthly_panel)

# Create plots
plots <- create_soe_plots(monthly_panel)

# Export results
cat("\n=== EXPORTING RESULTS ===\n")
write.csv(daily_panel, "soe_daily_panel.csv", row.names = FALSE)
write.csv(monthly_panel, "soe_monthly_panel.csv", row.names = FALSE)

cat("Files exported:\n")
cat("- soe_daily_panel.csv (daily panel)\n")
cat("- soe_monthly_panel.csv (monthly panel for analysis)\n")

cat("\n=== ACTUAL PANEL STRUCTURE (REAL DATA) ===\n")
cat("Column names in your panel:\n")
print(names(monthly_panel))

cat("\nActual sample of your data:\n")
sample_data <- monthly_panel %>%
  filter(division %in% c("St. James", "Portland")) %>%
  filter(year_month %in% c(as.Date("2016-01-01"), as.Date("2018-01-01"), as.Date("2018-02-01"))) %>%
  select(division, year_month, year, treated, post, soe_active, crime_total, robbery, shooting, murder)
print(sample_data)

cat("\nVariable definitions:\n")
cat("- treated: 1 if division ever gets SOE, 0 for pure controls\n")
cat("- post: 1 if date >= first SOE in that division\n") 
cat("- soe_active: 1 if SOE is currently active\n")
cat("- All crime numbers are ACTUAL counts from your data\n")

# Make objects available
cat("\nObjects created: daily_panel, monthly_panel, regression_results, plots\n")