# Create Parish-Year Observation Table for JSLC Data (2015-2019)
# Corrects 2018 parish coding inconsistency

library(haven)
library(dplyr)

cat("=== PARISH-YEAR OBSERVATION TABLE ===\n")

# Set main JSLC path
jslc_main_path <- "C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/JSLC/"

# Parish name mapping
parish_names <- c(
  "01" = "KINGSTON",
  "02" = "ST. ANDREW", 
  "03" = "ST. THOMAS",
  "04" = "PORTLAND",
  "05" = "ST. MARY",
  "06" = "ST. ANN",
  "07" = "TRELAWNY",
  "08" = "ST. JAMES",
  "09" = "HANOVER",
  "10" = "WESTMORELAND",
  "11" = "ST. ELIZABETH",
  "12" = "MANCHESTER",
  "13" = "CLARENDON",
  "14" = "ST. CATHERINE"
)

# Load all years
data_2015 <- readRDS(paste0(jslc_main_path, "JSLC_2015_Integrated.rds"))
data_2016 <- readRDS(paste0(jslc_main_path, "JSLC_2016_Integrated.rds"))
data_2017 <- readRDS(paste0(jslc_main_path, "JSLC_2017_Integrated.rds"))
data_2018 <- readRDS(paste0(jslc_main_path, "JSLC_2018_Integrated.rds"))
data_2019 <- readRDS(paste0(jslc_main_path, "JSLC_2019_Integrated.rds"))

# Standardize 2018 parish codes (convert single digit to double digit)
data_2018$PARISH_STD <- sprintf("%02d", as.numeric(data_2018$PARISH))

# Count observations by parish and year
parish_2015 <- table(data_2015$PARISH)
parish_2016 <- table(data_2016$Parish)
parish_2017 <- table(data_2017$Parish)
parish_2018 <- table(data_2018$PARISH_STD)  # Use standardized codes
parish_2019 <- table(data_2019$PARISH)

# Create comprehensive table
all_parish_codes <- sprintf("%02d", 1:14)

cat("Parish Name\t\tCode\t2015\t2016\t2017\t2018\t2019\tTotal\n")
cat("------------------------------------------------------------------------\n")

total_2015 <- 0
total_2016 <- 0  
total_2017 <- 0
total_2018 <- 0
total_2019 <- 0

for(code in all_parish_codes) {
  parish_name <- parish_names[code]
  
  count_2015 <- ifelse(code %in% names(parish_2015), parish_2015[code], 0)
  count_2016 <- ifelse(code %in% names(parish_2016), parish_2016[code], 0)
  count_2017 <- ifelse(code %in% names(parish_2017), parish_2017[code], 0)
  count_2018 <- ifelse(code %in% names(parish_2018), parish_2018[code], 0)
  count_2019 <- ifelse(code %in% names(parish_2019), parish_2019[code], 0)
  
  total_parish <- count_2015 + count_2016 + count_2017 + count_2018 + count_2019
  
  total_2015 <- total_2015 + count_2015
  total_2016 <- total_2016 + count_2016
  total_2017 <- total_2017 + count_2017
  total_2018 <- total_2018 + count_2018
  total_2019 <- total_2019 + count_2019
  
  cat(sprintf("%-15s\t%s\t%d\t%d\t%d\t%d\t%d\t%d\n", 
              parish_name, code, count_2015, count_2016, count_2017, count_2018, count_2019, total_parish))
}

grand_total <- total_2015 + total_2016 + total_2017 + total_2018 + total_2019

cat("------------------------------------------------------------------------\n")
cat(sprintf("%-15s\t%s\t%d\t%d\t%d\t%d\t%d\t%d\n", 
            "TOTAL", "  ", total_2015, total_2016, total_2017, total_2018, total_2019, grand_total))

# Create long format table for easy analysis
cat("\n=== LONG FORMAT TABLE ===\n")
cat("Parish_Name\t\tParish_Code\tYear\tObservations\n")
cat("------------------------------------------------\n")

for(code in all_parish_codes) {
  parish_name <- parish_names[code]
  
  count_2015 <- ifelse(code %in% names(parish_2015), parish_2015[code], 0)
  count_2016 <- ifelse(code %in% names(parish_2016), parish_2016[code], 0)
  count_2017 <- ifelse(code %in% names(parish_2017), parish_2017[code], 0)
  count_2018 <- ifelse(code %in% names(parish_2018), parish_2018[code], 0)
  count_2019 <- ifelse(code %in% names(parish_2019), parish_2019[code], 0)
  
  cat(sprintf("%-15s\t%s\t\t2015\t%d\n", parish_name, code, count_2015))
  cat(sprintf("%-15s\t%s\t\t2016\t%d\n", parish_name, code, count_2016))
  cat(sprintf("%-15s\t%s\t\t2017\t%d\n", parish_name, code, count_2017))
  cat(sprintf("%-15s\t%s\t\t2018\t%d\n", parish_name, code, count_2018))
  cat(sprintf("%-15s\t%s\t\t2019\t%d\n", parish_name, code, count_2019))
  cat("\n")
}

# Analysis for SOE research
cat("=== SOE ANALYSIS IMPLICATIONS ===\n")
cat("High-density parishes (likely to exclude):\n")

# Calculate parish totals for ranking
parish_totals <- c()
for(code in all_parish_codes) {
  count_2015 <- ifelse(code %in% names(parish_2015), parish_2015[code], 0)
  count_2016 <- ifelse(code %in% names(parish_2016), parish_2016[code], 0)
  count_2017 <- ifelse(code %in% names(parish_2017), parish_2017[code], 0)
  count_2018 <- ifelse(code %in% names(parish_2018), parish_2018[code], 0)
  count_2019 <- ifelse(code %in% names(parish_2019), parish_2019[code], 0)
  
  total_parish <- count_2015 + count_2016 + count_2017 + count_2018 + count_2019
  parish_totals <- c(parish_totals, total_parish)
}

names(parish_totals) <- all_parish_codes
parish_totals_sorted <- sort(parish_totals, decreasing = TRUE)

for(i in 1:3) {
  code <- names(parish_totals_sorted)[i]
  parish_name <- parish_names[code]
  count <- parish_totals_sorted[i]
  cat(sprintf("%d. %s (%s): %d observations\n", i, parish_name, code, count))
}

# Calculate exclusion impact
kingston_standrew <- parish_totals["01"] + parish_totals["02"]
remaining_total <- grand_total - kingston_standrew

cat(sprintf("\nIf excluding Kingston + St. Andrew:\n"))
cat(sprintf("- Excluded observations: %d (%.1f%%)\n", kingston_standrew, 100*kingston_standrew/grand_total))
cat(sprintf("- Remaining observations: %d across 12 parishes\n", remaining_total))
cat(sprintf("- Average per parish per year: %.0f observations\n", remaining_total/(12*5)))
