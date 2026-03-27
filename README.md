# soe-crime-analysis
Causal analysis of SOEs on crime and household outcomes in Jamaica
# Evaluating the Impact of States of Emergency (SOEs) on Crime and Economic Outcomes in Jamaica

## Overview

This project analyzes the causal impact of **States of Emergency (SOEs)** in Jamaica on both:

* Crime outcomes (division-level data)
* Household welfare outcomes (Jamaica Survey of Living Conditions - JSLC)

The goal is to understand whether aggressive public security policies reduce crime and how they affect economic well-being.

---

## Research Questions

* Do SOEs reduce crime rates?
* What are the short-run effects on:

  * Employment?
  * Household welfare (food security)?
* Are effects consistent across regions and over time?

---

## Data Sources

* **Crime Data (2016–2020)**
  Division-level monthly crime data from the Jamaica Constabulary Force (JCF)

* **SOE & ZOSO Intervention Data**
  Dates and locations of policy interventions

* **JSLC Household Survey (2014–2021)**
  Repeated cross-section household data including:

  * Employment
  * Income proxies
  * Food security
  * Demographics

---

## Methodology

### 1. Difference-in-Differences (DiD)

* Two-way fixed effects (parish and year)
* Clustered standard errors
* Robustness checks with:

  * Alternative treatment definitions
  * Treatment intensity
  * Dynamic/event-study effects

### 2. Generalized Synthetic Control (GSC)

* Accounts for time-varying unobserved confounders
* Cross-validation used to select latent factors
* Provides counterfactual crime trends

---

## Key Results (Preliminary)

* SOEs are associated with **reductions in crime rates**
* Effects on employment are **limited or statistically insignificant**
* Some evidence of **short-run improvements in food security**

---

## Project Structure

```
.
├── run_all.R                  # Main pipeline script
├── R/                         # All scripts
│   ├── 01_load_data.R
│   ├── 02_clean_data.R
│   ├── 03_build_panel.R
│   ├── 04_did_analysis.R
│   ├── 05_gsynth_analysis.R
│
├── data/
│   ├── raw/                   # (Not included in repo)
│   ├── processed/
│
├── output/
│   ├── tables/
│   ├── figures/
│
└── README.md
```

---

## How to Run the Project

### 1. Clone the repository

```bash
git clone https://github.com/YOUR_USERNAME/soe-crime-analysis.git
cd soe-crime-analysis
```

### 2. Install required R packages

```r
install.packages(c(
  "dplyr", "tidyr", "ggplot2", "fixest",
  "haven", "readr", "stringr", "gsynth"
))
```

### 3. Run the full pipeline

```r
source("run_all.R")
```

---

## Notes on Data

* Raw data files are not included due to size and access restrictions
* File paths must be updated to match your local environment

---

## Skills Demonstrated

* Causal inference (DiD, synthetic control)
* Panel data construction
* Data cleaning and harmonization
* Applied econometrics in R
* Policy evaluation

---

## Future Improvements

* Incorporate individual-level heterogeneity
* Extend to longer-term outcomes
* Improve geospatial matching of treatment exposure

---

## Author

Kerry Ann Purcell
PhD Candidate in Economics
University of Arkansas

---
