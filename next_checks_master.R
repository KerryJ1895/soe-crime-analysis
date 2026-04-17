## ============================================================
## next_checks_master.R
##   - GSC: ON/OFF vs absorbing, log(1+y), COVID trim
##   - Robust GSC SEs: parametric, nonparametric, jackknifeuuuuuutttttttttt
##   - Sun–Abraham (fixest): event studies + pretrend Wald tests
##   - Callaway–Sant’Anna (did): overall + dynamic plots
##   - Unit-placebo permutation for GSC absorbing
##   - All outputs are written to proj_dir as CSV/PNG/TXT
## ============================================================

## ---- Packages ----
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(lubridate)
  library(gsynth); library(ggplot2)
  library(fixest)   # Sun–Abraham (sunab), iplot, wald, coefplot
  library(did)      # Callaway–Sant’Anna
})

## ---- Paths ----
proj_dir   <- "C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/gsynth_offical"
crime_file <- "C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/Crime file 2016-2020.xlsx"
soe_file   <- "C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/ZOSO_and_SOE.xlsx"
setwd(proj_dir)

## ---- Helpers ----
norm <- function(x) gsub("\\s+", " ", trimws(toupper(x)))

# Safe plotter that deletes file if plotting errors
plot_safe <- function(expr, file, w = 1200, h = 700) {
  tryCatch({
    png(file, width = w, height = h, bg = "white")
    on.exit(dev.off(), add = TRUE)
    force(expr)
    TRUE
  }, error = function(e) {
    if (file.exists(file)) file.remove(file)
    message("Skipped plot ", file, ": ", e$message)
    FALSE
  })
}

# One-shot GSC runner with exports
run_gsynth <- function(df, inference = "parametric", r_fixed = NULL, suffix) {
  # df must have: division, year_month, crime_total, treated
  if (is.null(r_fixed)) {
    fit <- gsynth(crime_total ~ treated, data = df, index = c("division","year_month"),
                  force = "two-way", CV = TRUE, r = c(0,5),
                  se = TRUE, inference = inference, nboots = 1000, parallel = FALSE)
    rsel <- fit$r.cv
  } else {
    fit <- gsynth(crime_total ~ treated, data = df, index = c("division","year_month"),
                  force = "two-way", CV = FALSE, r = r_fixed,
                  se = TRUE, inference = inference, nboots = 1000, parallel = FALSE)
    rsel <- r_fixed
  }
  
  v <- as.numeric(fit$est.avg)   # [ATT, SE, CI_low, CI_up, p]
  att <- v[1]; se <- v[2]; ci_l <- v[3]; ci_u <- v[4]; p <- v[5]
  
  # ATT table
  write.csv(data.frame(
    ATT = round(att, 2), SE = round(se, 2),
    CI_lower = round(ci_l, 2), CI_upper = round(ci_u, 2),
    p_value = signif(p, 3), Factors = rsel
  ), paste0("att_", suffix, ".csv"), row.names = FALSE)
  
  # CV table
  if (!is.null(fit$cv)) {
    write.csv(
      data.frame(Factors = seq_along(fit$cv),
                 MSPE = round(fit$cv, 3),
                 Selected = ifelse(seq_along(fit$cv) == rsel, "*", "")),
      paste0("cv_", suffix, ".csv"), row.names = FALSE
    )
  }
  
  # Plots (reliably supported types)
  plot_safe(plot(fit, type = "counterfactual"), paste0("trends_",    suffix, ".png"))
  plot_safe(plot(fit, type = "ct"),             paste0("eventtime_", suffix, ".png"))
  plot_safe(plot(fit, type = "gap"),            paste0("gaps_",      suffix, ".png"))
  
  # Dynamic effects table if present
  if (!is.null(fit$est.beta)) {
    eb <- as.data.frame(fit$est.beta)
    colnames(eb)[1:5] <- c("ATT","SE","CI_L","CI_U","p")
    if (ncol(eb) >= 6) colnames(eb)[6] <- "N_tr"
    eb$period_index <- seq_len(nrow(eb))
    write.csv(eb, paste0("effects_over_time_", suffix, ".csv"), row.names = FALSE)
  }
  
  invisible(list(fit = fit, r = rsel, att = att, se = se))
}

## ============================================================
## 1) Load data & build panel
## ============================================================
crime_raw <- read_excel(crime_file) %>%
  mutate(
    DIVISION = norm(DIVISION),
    DATE_COMMITTED = as.Date(DATE_COMMITTED),
    year_month = as.Date(floor_date(DATE_COMMITTED, "month"))
  )

all_months <- seq(as.Date("2016-01-01"), as.Date("2020-12-01"), by = "month")
divisions  <- sort(unique(crime_raw$DIVISION))

panel <- tidyr::expand_grid(DIVISION = divisions, year_month = all_months) %>%
  left_join(
    crime_raw %>% count(DIVISION, year_month, name = "crime_total"),
    by = c("DIVISION","year_month")
  ) %>%
  mutate(crime_total = ifelse(is.na(crime_total), 0L, crime_total)) %>%
  rename(division = DIVISION)

# SOE schedule
soe_info <- read_excel(soe_file, sheet = "SOE") %>%
  transmute(
    division  = norm(Division),
    soe_start = as.Date(SOE_Effective_Date),
    soe_end   = as.Date(ifelse(is.na(SOE_End_Date), "2020-12-31", as.character(SOE_End_Date)))
  )

# Monthly ON/OFF treatment indicator
soe_monthly <- soe_info %>%
  rowwise() %>%
  mutate(month_seq = list(seq(floor_date(soe_start, "month"),
                              floor_date(soe_end,   "month"), by = "1 month"))) %>%
  unnest(month_seq) %>%
  transmute(division, year_month = month_seq, treated_onoff = 1L) %>%
  distinct()

panel <- panel %>%
  left_join(soe_monthly, by = c("division","year_month")) %>%
  mutate(treated_onoff = ifelse(is.na(treated_onoff), 0L, treated_onoff))

# Absorbing indicator = 1 from first treated month onward
first_on <- panel %>%
  group_by(division) %>%
  summarise(first_treat = if (any(treated_onoff == 1)) min(year_month[treated_onoff == 1]) else as.Date(NA),
            .groups = "drop")

panel <- panel %>%
  left_join(first_on, by = "division") %>%
  mutate(treated_absorb = ifelse(!is.na(first_treat) & year_month >= first_treat, 1L, 0L)) %>%
  arrange(division, year_month)

# Numeric time index for DiD flavors
panel <- panel %>% mutate(time_id = match(year_month, sort(unique(year_month))))

# COVID sensitivity sets
panel_pre20 <- panel %>% filter(year_month <= as.Date("2020-02-01"))

# Scale variant
panel <- panel %>% mutate(crime_log1p = log1p(crime_total))

cat("Panel rows:", nrow(panel),
    " | Divisions:", length(unique(panel$division)),
    " | Months:", length(unique(panel$year_month)),
    " | Share treated (ON/OFF):", round(mean(panel$treated_onoff), 3), "\n")

## ============================================================
## 2) GSC: ON/OFF vs absorbing; log(1+y); COVID trim
## ============================================================
# (c1) ON/OFF (levels)
res_on_param <- run_gsynth(
  panel %>% transmute(division, year_month, crime_total, treated = treated_onoff),
  inference = "parametric", r_fixed = NULL, suffix = "gsc_onoff_parametric"
)

# (c2) ABSORBING (levels) – primary
res_ab_param <- run_gsynth(
  panel %>% transmute(division, year_month, crime_total, treated = treated_absorb),
  inference = "parametric", r_fixed = NULL, suffix = "gsc_absorb_parametric"
)

# (b) Scale check: ABSORBING with log(1+y)
res_ab_log_param <- run_gsynth(
  panel %>% transmute(division, year_month, crime_total = crime_log1p, treated = treated_absorb),
  inference = "parametric", r_fixed = NULL, suffix = "gsc_absorb_log_parametric"
)

# (a) COVID sensitivity: Trim at 2020-02 (ABSORBING, levels)
invisible(run_gsynth(
  panel_pre20 %>% transmute(division, year_month, crime_total, treated = treated_absorb),
  inference = "parametric", r_fixed = NULL, suffix = "gsc_absorb_trim2020Feb"
))

## ---- Robust GSC SEs for absorbing spec (NP + Jackknife, reuse CV-selected r) ----
res_ab_np <- run_gsynth(
  panel %>% transmute(division, year_month, crime_total, treated = treated_absorb),
  inference = "nonparametric", r_fixed = res_ab_param$r, suffix = "gsc_absorb_nonparametric"
)
res_ab_jk <- run_gsynth(
  panel %>% transmute(division, year_month, crime_total, treated = treated_absorb),
  inference = "jackknife", r_fixed = res_ab_param$r, suffix = "gsc_absorb_jackknife"
)

# Collect robustness summary
gsc_robust <- data.frame(
  Spec     = c("ON/OFF param", "Absorbing param", "Absorb log1p param",
               "Absorbing nonparam", "Absorbing jackknife"),
  ATT      = round(c(res_on_param$att, res_ab_param$att, res_ab_log_param$att,
                     res_ab_np$att,    res_ab_jk$att), 2),
  SE       = round(c(res_on_param$se,  res_ab_param$se,  res_ab_log_param$se,
                     res_ab_np$se,     res_ab_jk$se), 2),
  Factors  = c(res_on_param$r, res_ab_param$r, res_ab_log_param$r, res_ab_np$r, res_ab_jk$r)
)
write.csv(gsc_robust, "gsc_robustness_summary.csv", row.names = FALSE)

## ============================================================
## 3) Sun–Abraham (fixest): event studies + pretrend tests
## ============================================================
# Cohort G (first-treated time_id), NA for never-treated
g_first <- panel %>%
  group_by(division) %>%
  summarise(G = ifelse(any(treated_absorb == 1),
                       min(time_id[treated_absorb == 1]),
                       NA_integer_),
            .groups = "drop")

did_sa_df <- panel %>% left_join(g_first, by = "division")

# Levels
sa_mod <- feols(crime_total ~ sunab(G, time_id) | division + time_id,
                data = did_sa_df, cluster = ~ division)
etable(sa_mod, file = "sunab_table_levels.txt")

# Event-study plot (silenced console)
plot_safe({
  invisible(iplot(sa_mod, ref.line = 0,
                  main = "Sun–Abraham event study (levels)",
                  xlab = "Event time (months relative to first SOE)",
                  ylab = "Coefficient"))
}, "sunab_eventstudy_levels.png")

# Export ES table from coefplot
sa_es_levels <- as.data.frame(coefplot(sa_mod, plot = FALSE)$coefs)
# Columns typically: estimate, se, ci_low, ci_high, relative_time, coefnames...
write.csv(sa_es_levels, "sunab_eventstudy_levels.csv", row.names = TRUE)

# Pretrend joint Wald test (all relative_time < 0 == 0)
cpL <- coefplot(sa_mod, plot = FALSE)$coefs
pre_names_L <- rownames(cpL)[cpL[, "relative_time"] < 0]
waldL <- tryCatch(
  wald(sa_mod, keep = paste(pre_names_L, collapse = "|")),
  error = function(e) NULL
)
if (!is.null(waldL)) {
  sink("sunab_pretrend_test_levels.txt"); print(waldL); sink()
  write.csv(
    data.frame(
      stat = unname(waldL$stat), df1 = waldL$df[1], df2 = waldL$df[2],
      p_value = waldL$p.value, n_pre = length(pre_names_L)
    ),
    "sunab_pretrend_test_levels.csv", row.names = FALSE
  )
}

# Log(1+y)
sa_mod_log <- feols(crime_log1p ~ sunab(G, time_id) | division + time_id,
                    data = did_sa_df, cluster = ~ division)
etable(sa_mod_log, file = "sunab_table_log1p.txt")
plot_safe({
  invisible(iplot(sa_mod_log, ref.line = 0,
                  main = "Sun–Abraham event study (log(1+y))",
                  xlab = "Event time (months relative to first SOE)",
                  ylab = "Coefficient (log points)"))
}, "sunab_eventstudy_log1p.png")
sa_es_log <- as.data.frame(coefplot(sa_mod_log, plot = FALSE)$coefs)
write.csv(sa_es_log, "sunab_eventstudy_log1p.csv", row.names = TRUE)

cpLG <- coefplot(sa_mod_log, plot = FALSE)$coefs
pre_names_LG <- rownames(cpLG)[cpLG[, "relative_time"] < 0]
waldLG <- tryCatch(
  wald(sa_mod_log, keep = paste(pre_names_LG, collapse = "|")),
  error = function(e) NULL
)
if (!is.null(waldLG)) {
  sink("sunab_pretrend_test_log1p.txt"); print(waldLG); sink()
  write.csv(
    data.frame(
      stat = unname(waldLG$stat), df1 = waldLG$df[1], df2 = waldLG$df[2],
      p_value = waldLG$p.value, n_pre = length(pre_names_LG)
    ),
    "sunab_pretrend_test_log1p.csv", row.names = FALSE
  )
}

# COVID-trimmed SA (levels)
sa_mod_trim <- feols(crime_total ~ sunab(G, time_id) | division + time_id,
                     data = did_sa_df %>% filter(year_month <= as.Date("2020-02-01")),
                     cluster = ~ division)
etable(sa_mod_trim, file = "sunab_table_trim2020Feb.txt")

## ============================================================
## 4) Callaway–Sant’Anna (did): overall + dynamic
## ============================================================
did_df <- did_sa_df %>%
  mutate(division_id = as.integer(factor(division)),
         G_cs = ifelse(is.na(G), 0L, G))

cs_att <- att_gt(
  yname  = "crime_total",
  tname  = "time_id",
  idname = "division_id",
  gname  = "G_cs",
  data   = did_df,
  panel  = TRUE,
  control_group = "nevertreated",
  allow_unbalanced_panel = TRUE
)

# Overall ATT (NOTE: type = "simple" for overall)
cs_overall  <- aggte(cs_att, type = "simple")
cs_dynamic  <- aggte(cs_att, type = "dynamic")
cs_group    <- aggte(cs_att, type = "group")
cs_calendar <- aggte(cs_att, type = "calendar")

# Export overall
write.csv(
  data.frame(
    ATT = cs_overall$overall.att,
    SE  = cs_overall$overall.se,
    CI_low = cs_overall$overall.att - 1.96 * cs_overall$overall.se,
    CI_up  = cs_overall$overall.att + 1.96 * cs_overall$overall.se
  ),
  "csdid_overall_levels.csv", row.names = FALSE
)

# Dynamic event-time plot
plot_safe({
  df <- data.frame(
    et  = cs_dynamic$egt,
    att = cs_dynamic$att.egt,
    low = cs_dynamic$att.egt - 1.96 * cs_dynamic$se.egt,
    up  = cs_dynamic$att.egt + 1.96 * cs_dynamic$se.egt
  )
  ggplot(df, aes(et, att)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_ribbon(aes(ymin = low, ymax = up), alpha = .15) +
    geom_line() + geom_point() +
    labs(title = "Callaway–Sant’Anna event study (levels)",
         x = "Event time (months)", y = "ATT")
}, "csdid_eventstudy_levels.png")

# Export group & calendar summaries
write.csv(data.frame(group = cs_group$egt,
                     att = cs_group$att.egt,
                     se  = cs_group$se.egt),
          "csdid_by_group.csv", row.names = FALSE)

write.csv(data.frame(calendar_time = cs_calendar$egt,
                     att = cs_calendar$att.egt,
                     se  = cs_calendar$se.egt),
          "csdid_by_calendar.csv", row.names = FALSE)

## ============================================================
## 5) Unit-placebo permutation for GSC absorbing (empirical p)
## ============================================================
set.seed(123)
B <- 300  # increase for more precision if needed

treated_cohorts <- first_on %>% filter(!is.na(first_treat)) %>%
  transmute(division, first_treat)
all_divs  <- unique(panel$division)
n_treated <- nrow(treated_cohorts)
placebo_atts <- numeric(B)

for (b in seq_len(B)) {
  sampled <- sample(all_divs, n_treated, replace = FALSE)
  perm_map <- treated_cohorts %>%
    mutate(division_new = sampled) %>%
    select(division_new, first_treat) %>%
    rename(division = division_new)
  
  placebo_df <- panel %>%
    select(division, year_month, crime_total) %>%
    left_join(perm_map, by = "division") %>%
    mutate(treated = ifelse(!is.na(first_treat) & year_month >= first_treat, 1L, 0L))
  
  fit <- try(gsynth(crime_total ~ treated, data = placebo_df,
                    index = c("division","year_month"),
                    force = "two-way", CV = TRUE, r = c(0,5),
                    se = TRUE, inference = "parametric",
                    nboots = 200, parallel = FALSE),
             silent = TRUE)
  
  if (!inherits(fit, "try-error")) {
    placebo_atts[b] <- as.numeric(fit$est.avg[1])
  } else {
    placebo_atts[b] <- NA_real_
  }
}

placebo_atts <- placebo_atts[!is.na(placebo_atts)]
write.csv(data.frame(placebo_ATT = placebo_atts),
          "placebo_att_distribution.csv", row.names = FALSE)

real_att <- res_ab_param$att
p_emp <- (sum(placebo_atts <= real_att) + 1) / (length(placebo_atts) + 1)

plot_safe({
  hist(placebo_atts, breaks = 30,
       main = "Unit-placebo ATT distribution (GSC absorbing)",
       xlab = "ATT", col = "grey85", border = "white")
  abline(v = real_att, col = "red", lwd = 3)
  legend("topright", bty = "n",
         legend = paste("Real ATT =", round(real_att, 2),
                        "\nEmpirical p =", round(p_emp, 3)))
}, "placebo_hist_vs_real.png")

## ============================================================
## 6) Console summary
## ============================================================
cat("\n=== SUMMARY (outputs written to", proj_dir, ") ===\n",
    "\nGSC ON/OFF vs ABSORBING (parametric, levels):",
    "\n  ON/OFF   ATT =", round(res_on_param$att, 2),
    "\n  ABSORB   ATT =", round(res_ab_param$att, 2),
    "\n\nScale (log(1+y)) absorbing ATT =", round(res_ab_log_param$att, 2),
    "\n\nRobust SEs (absorbing):",
    "\n  Nonparam  SE =", round(res_ab_np$se, 2),
    "\n  Jackknife SE =", round(res_ab_jk$se, 2),
    "\n\nPlacebos: draws kept =", length(placebo_atts),
    " | Empirical p (ATT <= real) =", round(p_emp, 3), "\n")
