# STAGGERED DIFFERENCE-IN-DIFFERENCES FOR SOE ANALYSIS
# Focus on proper causal identification using the 'did' package

library(did)
library(dplyr)
library(ggplot2)
library(lubridate)

# Function to run Staggered DiD analysis
run_staggered_did_analysis <- function(monthly_panel) {
  cat("=== STAGGERED DIFFERENCE-IN-DIFFERENCES ANALYSIS ===\n")
  cat("Analyzing SOE treatment effects with proper causal identification\n\n")
  
  # Step 1: Prepare data for did package
  cat("Step 1: Preparing data for staggered DiD...\n")
  
  did_data <- monthly_panel %>%
    mutate(
      # Create numeric time period 
      time_period = as.numeric(factor(year_month)),
      # Create unit identifier
      unit_id = as.numeric(factor(division))
    )
  
  # Step 2: Identify first treatment period for each division
  cat("Step 2: Identifying treatment timing...\n")
  
  # Find when each division first gets SOE
  treatment_timing <- did_data %>%
    filter(soe_active == 1) %>%
    group_by(division, unit_id) %>%
    summarise(first_treat_period = min(time_period), .groups = "drop")
  
  # Add treatment timing to main data
  did_data <- did_data %>%
    left_join(treatment_timing, by = c("division", "unit_id")) %>%
    mutate(
      # Never-treated units get 0
      first_treat = ifelse(is.na(first_treat_period), 0, first_treat_period)
    )
  
  # Display treatment timing
  cat("Treatment timing summary:\n")
  timing_summary <- did_data %>%
    distinct(division, first_treat) %>%
    arrange(first_treat) %>%
    mutate(
      treatment_status = case_when(
        first_treat == 0 ~ "Never Treated",
        TRUE ~ paste("First SOE at period", first_treat)
      )
    )
  print(timing_summary)
  
  # Step 3: Run staggered DiD for Total Crimes
  cat("\nStep 3: Running staggered DiD for Total Crimes...\n")
  
  did_result_total <- att_gt(
    yname = "crime_total",
    tname = "time_period", 
    idname = "unit_id",
    gname = "first_treat",
    data = did_data,
    control_group = "nevertreated",
    clustervars = "unit_id",
    base_period = "universal",
    anticipation = 0,  # No anticipation effects
    biters = 1000,     # Bootstrap iterations
    cband = TRUE       # Confidence bands
  )
  
  cat("Group-Time Average Treatment Effects:\n")
  print(summary(did_result_total))
  
  # Step 4: Aggregate results
  cat("\nStep 4: Computing overall treatment effects...\n")
  
  # Overall ATT (simple aggregation)
  overall_att <- aggte(did_result_total, type = "simple")
  cat("Overall Average Treatment Effect:\n")
  print(summary(overall_att))
  
  # Dynamic effects (by time since treatment)
  dynamic_att <- aggte(did_result_total, type = "dynamic")
  cat("\nDynamic Treatment Effects:\n")
  print(summary(dynamic_att))
  
  # Calendar time effects
  calendar_att <- aggte(did_result_total, type = "calendar")
  cat("\nCalendar Time Effects:\n")
  print(summary(calendar_att))
  
  # Step 5: Visualizations
  cat("\nStep 5: Creating visualizations...\n")
  
  # Plot 1: Group-time effects
  p1 <- ggdid(did_result_total) +
    ggtitle("Group-Time Average Treatment Effects") +
    theme_minimal()
  print(p1)
  
  # Plot 2: Dynamic effects
  p2 <- ggdid(dynamic_att) +
    ggtitle("Dynamic Effects of SOE on Total Crimes") +
    xlab("Periods Since SOE Implementation") +
    theme_minimal()
  print(p2)
  
  # Plot 3: Calendar time effects
  p3 <- ggdid(calendar_att) +
    ggtitle("Calendar Time Effects") +
    theme_minimal()
  print(p3)
  
  # Step 6: Run analysis for specific crime types
  cat("\nStep 6: Analyzing specific crime types...\n")
  
  # Shooting
  cat("Analyzing shooting...\n")
  did_shooting <- att_gt(
    yname = "shooting",
    tname = "time_period", 
    idname = "unit_id",
    gname = "first_treat",
    data = did_data,
    control_group = "nevertreated",
    clustervars = "unit_id"
  )
  
  overall_shooting <- aggte(did_shooting, type = "simple")
  cat("Overall ATT - Shooting:\n")
  print(summary(overall_shooting))
  
  # Murder
  cat("\nAnalyzing murder...\n")
  did_murder <- att_gt(
    yname = "murder",
    tname = "time_period", 
    idname = "unit_id",
    gname = "first_treat",
    data = did_data,
    control_group = "nevertreated",
    clustervars = "unit_id"
  )
  
  overall_murder <- aggte(did_murder, type = "simple")
  cat("Overall ATT - Murder:\n")
  print(summary(overall_murder))
  
  # Robbery
  cat("\nAnalyzing robbery...\n")
  did_robbery <- att_gt(
    yname = "robbery",
    tname = "time_period", 
    idname = "unit_id",
    gname = "first_treat",
    data = did_data,
    control_group = "nevertreated",
    clustervars = "unit_id"
  )
  
  overall_robbery <- aggte(did_robbery, type = "simple")
  cat("Overall ATT - Robbery:\n")
  print(summary(overall_robbery))
  
  # Step 7: Create results summary
  cat("\n=== STAGGERED DiD RESULTS SUMMARY ===\n")
  
  results_summary <- data.frame(
    Crime_Type = c("Total Crimes", "Shooting", "Murder", "Robbery"),
    ATT = c(
      overall_att$overall.att,
      overall_shooting$overall.att, 
      overall_murder$overall.att,
      overall_robbery$overall.att
    ),
    SE = c(
      overall_att$overall.se,
      overall_shooting$overall.se,
      overall_murder$overall.se, 
      overall_robbery$overall.se
    ),
    P_Value = c(
      2 * (1 - pnorm(abs(overall_att$overall.att / overall_att$overall.se))),
      2 * (1 - pnorm(abs(overall_shooting$overall.att / overall_shooting$overall.se))),
      2 * (1 - pnorm(abs(overall_murder$overall.att / overall_murder$overall.se))),
      2 * (1 - pnorm(abs(overall_robbery$overall.att / overall_robbery$overall.se)))
    )
  ) %>%
    mutate(
      CI_Lower = ATT - 1.96 * SE,
      CI_Upper = ATT + 1.96 * SE,
      Significant = P_Value < 0.05
    )
  
  print(results_summary)
  
  # Step 8: Export results
  cat("\nStep 8: Exporting results...\n")
  write.csv(results_summary, "staggered_did_results.csv", row.names = FALSE)
  write.csv(timing_summary, "soe_treatment_timing.csv", row.names = FALSE)
  
  cat("Files exported:\n")
  cat("- staggered_did_results.csv\n")
  cat("- soe_treatment_timing.csv\n")
  
  # Return results for further analysis
  return(list(
    total_crimes = list(
      group_time = did_result_total,
      overall = overall_att,
      dynamic = dynamic_att,
      calendar = calendar_att
    ),
    shooting = list(
      group_time = did_shooting,
      overall = overall_shooting
    ),
    murder = list(
      group_time = did_murder,
      overall = overall_murder
    ),
    robbery = list(
      group_time = did_robbery,
      overall = overall_robbery
    ),
    summary = results_summary,
    data = did_data,
    plots = list(p1 = p1, p2 = p2, p3 = p3)
  ))
}

# Function to check pre-treatment parallel trends
check_parallel_trends <- function(did_results) {
  cat("\n=== CHECKING PARALLEL TRENDS ASSUMPTION ===\n")
  
  # Extract pre-treatment effects from dynamic results
  dynamic_results <- did_results$total_crimes$dynamic
  
  # Get the dynamic effects data
  dynamic_data <- data.frame(
    event_time = dynamic_results$egt,
    att = dynamic_results$att.egt,
    se = dynamic_results$se.egt
  ) %>%
    mutate(
      ci_lower = att - 1.96 * se,
      ci_upper = att + 1.96 * se,
      significant = abs(att) > 1.96 * se
    )
  
  # Focus on pre-treatment periods
  pre_treatment <- dynamic_data %>%
    filter(event_time < 0)
  
  cat("Pre-treatment effects (should be close to zero):\n")
  print(pre_treatment)
  
  # Test if pre-treatment effects are jointly zero
  pre_treatment_test <- all(abs(pre_treatment$att) < 1.96 * pre_treatment$se)
  
  cat("\nParallel trends assessment:\n")
  if(pre_treatment_test) {
    cat("✓ Pre-treatment effects are not significantly different from zero\n")
    cat("✓ Parallel trends assumption appears satisfied\n")
  } else {
    cat("⚠ Some pre-treatment effects are significant\n")
    cat("⚠ Parallel trends assumption may be violated\n")
  }
  
  return(list(
    pre_treatment_effects = pre_treatment,
    parallel_trends_satisfied = pre_treatment_test
  ))
}

# Usage instructions
cat("STAGGERED DIFFERENCE-IN-DIFFERENCES ANALYSIS\n")
cat(rep("=", 55), "\n")
cat("\nTo run the analysis:\n")
cat("1. did_results <- run_staggered_did_analysis(monthly_panel)\n")
cat("2. trends_check <- check_parallel_trends(did_results)\n")
cat("\nThis will provide:\n")
cat("- Proper causal identification of SOE effects\n")
cat("- Treatment effects by crime type\n") 
cat("- Dynamic effects over time\n")
cat("- Parallel trends testing\n")
cat("- Publication-ready results\n")

# Run the staggered DiD analysis
did_results <- run_staggered_did_analysis(monthly_panel)

# Check parallel trends assumption  
trends_check <- check_parallel_trends(did_results)

# Run this to see which specific periods are problematic
print(trends_check$pre_treatment_effects[trends_check$pre_treatment_effects$significant == TRUE,])
