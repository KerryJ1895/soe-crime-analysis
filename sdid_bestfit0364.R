# Option 2: Remove poor-fitting control units
cat("\n=== TRYING WITH BETTER CONTROL UNITS ===\n")

# Identify which control units fit better
control_quality <- data.frame()

for(i in 1:N0) {
  control_trend <- outcome_matrix[i, 1:T0]
  treated_avg <- colMeans(outcome_matrix[(N0+1):nrow(outcome_matrix), 1:T0])
  
  correlation <- cor(control_trend, treated_avg)
  rmse <- sqrt(mean((control_trend - treated_avg)^2))
  
  control_quality <- rbind(control_quality, data.frame(
    control_unit = i,
    correlation = correlation,
    rmse = rmse
  ))
}

# Add division names
control_quality$division <- synthdid_data %>% 
  distinct(unit_id, division) %>%
  arrange(match(unit_id, ordered_units)) %>%
  head(N0) %>%
  pull(division)

print(control_quality)

# Keep only top 5 control units by correlation
good_controls <- control_quality %>%
  arrange(desc(correlation)) %>%
  head(5) %>%
  pull(control_unit)

cat("Using only these", length(good_controls), "best control units:\n")
print(control_quality[control_quality$control_unit %in% good_controls, ])

# Create new matrices with only good controls
outcome_matrix_filtered <- rbind(
  outcome_matrix[good_controls, ],  # Good controls only
  outcome_matrix[(N0+1):nrow(outcome_matrix), ]  # All treated units
)

treatment_matrix_filtered <- rbind(
  treatment_matrix[good_controls, ],
  treatment_matrix[(N0+1):nrow(treatment_matrix), ]
)

N0_filtered <- length(good_controls)

# Re-run with filtered controls
synthdid_est_filtered <- synthdid_estimate(outcome_matrix_filtered, treatment_matrix_filtered, 
                                           T0 = T0, N0 = N0_filtered)

cat("Filtered estimate:", round(synthdid_est_filtered, 3), "\n")

# Check fit quality
weights_filtered <- attr(synthdid_est_filtered, 'weights')
trend_data_filtered <- data.frame(
  time_period = 1:ncol(outcome_matrix_filtered),
  treated_actual = colMeans(outcome_matrix_filtered[(N0_filtered+1):nrow(outcome_matrix_filtered), ]),
  synthetic_control = as.numeric(t(weights_filtered$omega) %*% outcome_matrix_filtered[1:N0_filtered, ])
) %>%
  mutate(year_month = unique(synthdid_data$year_month)[time_period])

pre_treatment_filtered <- trend_data_filtered %>%
  filter(time_period <= T0) %>%
  mutate(difference = treated_actual - synthetic_control)

pre_correlation_filtered <- cor(pre_treatment_filtered$treated_actual, pre_treatment_filtered$synthetic_control)
pre_rmse_filtered <- sqrt(mean(pre_treatment_filtered$difference^2))

cat("Filtered pre-treatment correlation:", round(pre_correlation_filtered, 3), "\n")
cat("Filtered pre-treatment RMSE:", round(pre_rmse_filtered, 3), "\n")

# Plot the improved fit
p_trends_filtered <- ggplot(trend_data_filtered, aes(x = year_month)) +
  geom_line(aes(y = treated_actual, color = "Treated Units (Actual)"), size = 1.2) +
  geom_line(aes(y = synthetic_control, color = "Synthetic Control"), size = 1.2, linetype = "dashed") +
  geom_vline(xintercept = unique(synthdid_data$year_month)[T0 + 1], 
             linetype = "solid", color = "red", alpha = 0.7, size = 1) +
  scale_color_manual(values = c("Treated Units (Actual)" = "blue", 
                                "Synthetic Control" = "orange")) +
  labs(
    title = "Synthetic DiD: Best Controls Only",
    subtitle = paste("Pre-treatment correlation:", round(pre_correlation_filtered, 3)),
    x = "Date",
    y = "Average Monthly Total Crimes"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")

print(p_trends_filtered)
