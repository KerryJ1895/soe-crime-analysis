# =======================================================
# Generalized Synthetic Control: Jamaica SOE Crime Analysis
# =======================================================
# All inputs/outputs in:
#   C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/gsynth_offical
# =======================================================

## ---- Setup ----
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(gsynth)
library(ggplot2)

proj_dir   <- "C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/gsynth_offical"
crime_file <- "C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/Crime file 2016-2020.xlsx"
soe_file   <- "C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/ZOSO_and_SOE.xlsx"

setwd(proj_dir)

# Normalize division strings (trim, collapse spaces, uppercase)
norm <- function(x) gsub("\\s+", " ", trimws(toupper(x)))

## ---- STEP 1: Load & normalize crime incidents ----
crime_raw <- read_excel(crime_file)

crime_raw <- crime_raw %>%
  mutate(
    DIVISION       = norm(DIVISION),
    DATE_COMMITTED = as.Date(DATE_COMMITTED),
    year_month     = as.Date(floor_date(DATE_COMMITTED, "month"))
  )

# Complete division × month panel (zero-fill)
all_months <- seq(as.Date("2016-01-01"), as.Date("2020-12-01"), by = "month")
divisions  <- sort(unique(crime_raw$DIVISION))
panel_grid <- tidyr::expand_grid(DIVISION = divisions, year_month = all_months)

crime_monthly <- panel_grid %>%
  left_join(
    crime_raw %>% group_by(DIVISION, year_month) %>% summarise(crime_total = n(), .groups = "drop"),
    by = c("DIVISION", "year_month")
  ) %>%
  mutate(crime_total = ifelse(is.na(crime_total), 0L, crime_total))

## ---- STEP 2: Load & normalize SOE schedule ----
soe_info <- read_excel(soe_file, sheet = "SOE") %>%
  rename(
    division  = Division,
    soe_start = SOE_Effective_Date,
    soe_end   = SOE_End_Date
  ) %>%
  mutate(
    division  = norm(division),
    soe_start = as.Date(soe_start),
    soe_end   = as.Date(ifelse(is.na(soe_end), "2020-12-31", as.character(soe_end)))
  )

## ---- STEP 3: Expand SOE schedule into monthly rows & deduplicate ----
soe_monthly <- soe_info %>%
  rowwise() %>%
  mutate(month_seq = list(seq(floor_date(soe_start, "month"),
                              floor_date(soe_end,   "month"),
                              by = "1 month"))) %>%
  unnest(month_seq) %>%
  rename(year_month = month_seq) %>%
  mutate(treated = 1L) %>%
  distinct(division, year_month, .keep_all = TRUE) %>%
  select(division, year_month, treated)

## ---- STEP 4: Merge treatment with panel ----
crime_monthly <- crime_monthly %>%
  rename(division = DIVISION) %>%
  mutate(division = norm(division)) %>%
  left_join(soe_monthly, by = c("division", "year_month")) %>%
  mutate(treated = ifelse(is.na(treated), 0L, treated)) %>%
  arrange(division, year_month)

cat("Panel rows:", nrow(crime_monthly),
    " | Divisions:", length(unique(crime_monthly$division)),
    " | Months:", length(unique(crime_monthly$year_month)),
    " | Share treated:", round(mean(crime_monthly$treated), 3), "\n")

## ---- Helper: robust plotting (avoids blank PNGs) ----
gsynth_plot_safe <- function(obj, type, filename, width=1200, height=700) {
  tryCatch({
    png(filename, width = width, height = height, bg = "white")
    on.exit({ if (dev.cur() > 1) dev.off() }, add = TRUE)
    plot(obj, type = type)
    TRUE
  }, error = function(e) {
    if (dev.cur() > 1) dev.off()
    if (file.exists(filename)) file.remove(filename)
    message(sprintf("Skipped plot type '%s': %s", type, e$message))
    FALSE
  })
}

## ---- Helper: run gsynth + export with suffix ----
run_gsynth_and_export <- function(df, inference = "parametric", r_fixed = NULL, suffix = "parametric") {
  # Estimate
  if (is.null(r_fixed)) {
    fit <- gsynth(
      crime_total ~ treated,
      data = df, index = c("division", "year_month"),
      force = "two-way",
      CV = TRUE, r = c(0, 5),
      se = TRUE, inference = inference, nboots = 1000, parallel = FALSE
    )
  } else {
    fit <- gsynth(
      crime_total ~ treated,
      data = df, index = c("division", "year_month"),
      force = "two-way",
      CV = FALSE, r = r_fixed,
      se = TRUE, inference = inference, nboots = 1000, parallel = FALSE
    )
  }
  
  # Scalars from est.avg: ATT, SE, CI_low, CI_up, p
  v <- as.numeric(fit$est.avg)
  att  <- v[1]; se <- v[2]; ci_l <- v[3]; ci_u <- v[4]; p <- v[5]
  rsel <- if (!is.null(r_fixed)) r_fixed else fit$r.cv
  
  # Save ATT
  write.csv(data.frame(
    ATT = round(att, 2), SE = round(se, 2),
    CI_lower = round(ci_l, 2), CI_upper = round(ci_u, 2),
    p_value = signif(p, 3), Factors = rsel
  ), paste0("soe_gsynth_ATT_", suffix, ".csv"), row.names = FALSE)
  
  # Save CV results when available
  if (!is.null(fit$cv)) {
    cv_tab <- data.frame(
      Factors  = seq_along(fit$cv),
      MSPE     = round(fit$cv, 3),
      Selected = ifelse(seq_along(fit$cv) == rsel, "*", "")
    )
    write.csv(cv_tab, paste0("soe_gsynth_cv_results_", suffix, ".csv"), row.names = FALSE)
  }
  
  # Plots (supported types)
  unlink(paste0("soe_gsynth_trends_", suffix, ".png"), force = TRUE)
  unlink(paste0("soe_gsynth_ct_",     suffix, ".png"), force = TRUE)
  unlink(paste0("soe_gsynth_gap_",    suffix, ".png"), force = TRUE)
  gsynth_plot_safe(fit, "counterfactual", paste0("soe_gsynth_trends_", suffix, ".png"))
  gsynth_plot_safe(fit, "ct",            paste0("soe_gsynth_ct_",     suffix, ".png"))
  gsynth_plot_safe(fit, "gap",           paste0("soe_gsynth_gap_",    suffix, ".png"))
  
  # Diagnostics (pre-fit RMSE / corr if series exist)
  rmse_pre <- NA_real_; cor_pre <- NA_real_
  if (!is.null(fit$Y.tr) && !is.null(fit$Y.ct)) {
    ytr <- as.numeric(fit$Y.tr); yct <- as.numeric(fit$Y.ct)
    Ttot <- length(ytr)
    pre_idx <- seq_len(floor(Ttot/2))
    if (!is.null(fit$est.beta)) {
      post_start <- which(!is.na(fit$est.beta[,1]))[1]
      if (!is.na(post_start) && post_start > 1) pre_idx <- seq_len(post_start - 1)
    }
    if (length(pre_idx) >= 3) {
      diffs <- ytr[pre_idx] - yct[pre_idx]
      rmse_pre <- sqrt(mean(diffs^2, na.rm = TRUE))
      cor_pre  <- suppressWarnings(cor(ytr[pre_idx], yct[pre_idx], use = "pairwise.complete.obs"))
    }
  }
  write.csv(data.frame(
    RMSE_pre = ifelse(is.na(rmse_pre), NA, round(rmse_pre, 3)),
    Corr_pre = ifelse(is.na(cor_pre),  NA, round(cor_pre, 3)),
    ATT = round(att, 2), SE = round(se, 2),
    CI_lower = round(ci_l, 2), CI_upper = round(ci_u, 2),
    p_value = signif(p, 3), Factors = rsel
  ), paste0("soe_gsynth_diagnostics_", suffix, ".csv"), row.names = FALSE)
  
  # Dynamic effects (if present)
  if (!is.null(fit$est.beta)) {
    eb <- as.data.frame(fit$est.beta)
    colnames(eb)[1:5] <- c("ATT","SE","CI_L","CI_U","p")
    if (ncol(eb) >= 6) colnames(eb)[6] <- "N_tr"
    eb$period_index <- seq_len(nrow(eb))
    write.csv(eb, paste0("soe_gsynth_effects_over_time_", suffix, ".csv"), row.names = FALSE)
  }
  
  # Save model object
  save(fit, file = paste0("soe_gsynth_result_", suffix, ".RData"))
  
  invisible(list(fit = fit, rsel = rsel,
                 att = att, se = se, ci_l = ci_l, ci_u = ci_u, p = p))
}

## ---- STEP 5: Run PARAMETRIC (CV selects factors) ----
res_param <- run_gsynth_and_export(crime_monthly, inference = "parametric", r_fixed = NULL, suffix = "parametric")

## ---- STEP 6: Run NONPARAMETRIC (reuse same number of factors) ----
res_np <- run_gsynth_and_export(crime_monthly, inference = "nonparametric",
                                r_fixed = res_param$rsel, suffix = "nonparametric")

## ---- STEP 7: Run JACKKNIFE (reuse same number of factors) ----
res_jk <- run_gsynth_and_export(crime_monthly, inference = "jackknife",
                                r_fixed = res_param$rsel, suffix = "jackknife")

## ---- Console summary ----
cat("\n=== SUMMARY ===\n",
    "Factors (CV): ", res_param$rsel, "\n",
    "Parametric     ATT (se): ", round(res_param$att,2), " (", round(res_param$se,2), ")\n",
    "Nonparametric  ATT (se): ", round(res_np$att,2),    " (", round(res_np$se,2),    ")\n",
    "Jackknife      ATT (se): ", round(res_jk$att,2),    " (", round(res_jk$se,2),    ")\n",
    "Files written with suffixes: _parametric, _nonparametric, _jackknife\n", sep="")

message("\nRunning next-checks bundle (next_checks_master.R)...")
source(file.path("C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/gsynth_offical",
                 "next_checks_master.R"))

