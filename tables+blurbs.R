## ==== Build a clean main table + short blurbs ====
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(lubridate)
})

proj_dir   <- "C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/gsynth_offical"
crime_file <- "C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/Crime file 2016-2020.xlsx"
soe_file   <- "C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/ZOSO_and_SOE.xlsx"
setwd(proj_dir)

norm <- function(x) gsub("\\s+"," ", trimws(toupper(x)))
fmt  <- function(x) ifelse(is.na(x),"", sprintf("%.2f", x))
stars <- function(p){ ifelse(is.na(p),"",
                             ifelse(p<.001,"***", ifelse(p<.01,"**", ifelse(p<.05,"*","")))) }
read_att <- function(file, spec, outcome, inference){
  df <- read.csv(file, check.names = FALSE)
  tibble(Spec = spec, Outcome = outcome, Inference = inference,
         ATT = df$ATT[1], SE = df$SE[1],
         CI_lower = df$CI_lower[1], CI_upper = df$CI_upper[1],
         p_value = if("p_value" %in% names(df)) df$p_value[1] else NA_real_)
}

# Gather the GSC outputs you already created
tbl <- bind_rows(
  read_att("att_gsc_absorb_parametric.csv",    "GSC (Absorbing)", "Levels",     "Parametric"),
  read_att("att_gsc_absorb_nonparametric.csv","GSC (Absorbing)", "Levels",     "Nonparametric"),
  read_att("att_gsc_absorb_jackknife.csv",    "GSC (Absorbing)", "Levels",     "Jackknife"),
  read_att("att_gsc_onoff_parametric.csv",    "GSC (On/Off)",    "Levels",     "Parametric"),
  read_att("att_gsc_absorb_log_parametric.csv","GSC (Absorbing)","log(1+y)",   "Parametric"),
  read_att("att_gsc_absorb_trim2020Feb.csv",  "GSC (Absorbing)", "Levels (≤ Feb 2020)", "Parametric")
)

# Add Callaway–Sant’Anna overall ATT
if (file.exists("csdid_overall_levels.csv")) {
  cs <- read.csv("csdid_overall_levels.csv")
  tbl <- bind_rows(tbl, tibble(
    Spec="Callaway–Sant’Anna", Outcome="Levels", Inference="DR (overall)",
    ATT=cs$ATT[1], SE=cs$SE[1], CI_lower=cs$CI_low[1], CI_upper=cs$CI_up[1], p_value=NA_real_
  ))
}

# Compute % change for “levels”: ATT divided by pre-treatment mean in treated divisions
# (and for log(1+y): 100*(exp(ATT)-1))
crime_raw <- read_excel(crime_file) %>%
  mutate(DIVISION=norm(DIVISION), DATE_COMMITTED=as.Date(DATE_COMMITTED),
         year_month=as.Date(floor_date(DATE_COMMITTED,"month")))
all_months <- seq(as.Date("2016-01-01"), as.Date("2020-12-01"), by="month")
panel <- expand_grid(DIVISION=sort(unique(crime_raw$DIVISION)), year_month=all_months) %>%
  left_join(count(crime_raw, DIVISION, year_month, name="crime_total"),
            by=c("DIVISION","year_month")) %>%
  mutate(crime_total=ifelse(is.na(crime_total),0L,crime_total)) %>%
  rename(division=DIVISION)

soe_info <- read_excel(soe_file, sheet="SOE") %>%
  transmute(division=norm(Division),
            soe_start=as.Date(SOE_Effective_Date),
            soe_end=as.Date(ifelse(is.na(SOE_End_Date),"2020-12-31",as.character(SOE_End_Date))))
soe_monthly <- soe_info %>% rowwise() %>%
  mutate(month_seq=list(seq(floor_date(soe_start,"month"), floor_date(soe_end,"month"), by="1 month"))) %>%
  unnest(month_seq) %>% transmute(division, year_month=month_seq, treated=1L) %>% distinct()

panel2 <- panel %>% left_join(soe_monthly, by=c("division","year_month")) %>%
  mutate(treated=ifelse(is.na(treated),0L,treated))
first_treat <- panel2 %>% group_by(division) %>%
  summarise(first_treat = if (any(treated==1)) min(year_month[treated==1]) else as.Date(NA), .groups="drop")
pre_mean <- panel2 %>% left_join(first_treat, by="division") %>%
  filter(!is.na(first_treat), year_month < first_treat) %>%
  summarise(m = mean(crime_total, na.rm=TRUE)) %>% pull(m)

tbl <- tbl %>% mutate(
  Percent = ifelse(grepl("^log", Outcome),
                   100*(exp(ATT)-1),
                   100*ATT/pre_mean)
)

# Final pretty table (CSV + optional HTML via gt)
tbl_out <- tbl %>% mutate(
  ATT_se = paste0(fmt(ATT), " (", fmt(SE), ")", stars(p_value)),
  CI     = paste0("[", fmt(CI_lower), ", ", fmt(CI_upper), "]"),
  Percent= sprintf("%.1f%%", Percent)
) %>% select(Spec, Outcome, Inference, ATT_se, CI, Percent)

write.csv(tbl_out, "main_results_table.csv", row.names=FALSE)

if (requireNamespace("gt", quietly=TRUE)) {
  library(gt)
  gt_tbl <- tbl_out %>% gt() %>%
    tab_header(title="Main Effects & Robustness") %>%
    cols_label(Spec="Specification", Outcome="Outcome", Inference="SE/Estimator",
               ATT_se="ATT (SE)", CI="95% CI", Percent="% change") %>%
    tab_source_note(md("Notes: Levels %% = ATT / pre-treatment mean of treated divisions; log(1+y) %% = 100·(e^ATT−1). Stars: * p<0.05, ** p<0.01, *** p<0.001."))
  gtsave(gt_tbl, "main_results_table.html")
}

# Short blurbs (auto-filled) — good for slides
# (Recompute empirical placebo p using saved files if available)
p_emp <- try({
  placebos <- read.csv("placebo_att_distribution.csv")$placebo_ATT
  real_att <- read.csv("att_gsc_absorb_parametric.csv")$ATT[1]
  (sum(placebos <= real_att) + 1) / (length(placebos) + 1)
}, silent=TRUE); if (inherits(p_emp, "try-error")) p_emp <- NA_real_

abs_param   <- tbl %>% filter(Spec=="GSC (Absorbing)", Outcome=="Levels", Inference=="Parametric")
abs_jk      <- tbl %>% filter(Spec=="GSC (Absorbing)", Outcome=="Levels", Inference=="Jackknife")
abs_log     <- tbl %>% filter(Spec=="GSC (Absorbing)", grepl("^log",Outcome))
abs_log_pct <- tbl_out %>% filter(Spec=="GSC (Absorbing)", grepl("^log",Outcome)) %>% pull(Percent)

cat(
  sprintf("GSC absorbing (levels): ATT = %.2f (parametric SE = %.2f; jackknife SE = %.2f).",
          abs_param$ATT, abs_param$SE, abs_jk$SE),
  sprintf("\nGSC absorbing (log(1+y)): ATT = %.2f (~%s).",
          abs_log$ATT, abs_log_pct),
  if (!is.na(p_emp)) sprintf("\nPlacebo empirical p = %.3f.", p_emp) else "",
  "\nSee main_results_table.(csv/html) for all specs.\n",
  file="results_summary_blurbs.txt"
)
