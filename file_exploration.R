# Excel Files Data Exploration for Crime and SOE/ZOSO Analysis
# Load required libraries
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(stringr)

# File paths
crime_file <- "C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/Crime file 2016-2020.xlsx"
soe_file <- "C:/Users/kjpurcel/OneDrive - University of Arkansas/crime_SOE/ZOSO_and_SOE.xlsx"

# Function to explore Excel file structure
explore_excel_file <- function(file_path, file_description = "") {
  
  cat("\n", rep("=", 80), sep = "")
  cat("\nEXPLORING:", file_description)
  cat("\nFILE:", file_path)
  cat("\n", rep("=", 80), sep = "")
  
  tryCatch({
    # Get sheet names
    sheet_names <- excel_sheets(file_path)
    cat("\n\nAvailable sheets:", paste(sheet_names, collapse = ", "))
    
    all_data <- list()
    
    for (sheet_name in sheet_names) {
      cat("\n", rep("-", 60), sep = "")
      cat("\nSHEET:", sheet_name)
      cat("\n", rep("-", 60), sep = "")
      
      tryCatch({
        # Read the sheet
        df <- read_excel(file_path, sheet = sheet_name)
        all_data[[sheet_name]] <- df
        
        cat("\nShape:", nrow(df), "rows x", ncol(df), "columns")
        cat("\nColumns:", paste(names(df), collapse = ", "))
        
        # Check for missing values
        cat("\n\nMissing values per column:")
        missing_counts <- sapply(df, function(x) sum(is.na(x)))
        missing_pct <- round(missing_counts / nrow(df) * 100, 1)
        
        for (i in seq_along(missing_counts)) {
          if (missing_counts[i] > 0) {
            cat("\n  ", names(missing_counts)[i], ":", missing_counts[i], 
                "(", missing_pct[i], "%)")
          }
        }
        
        # Data types
        cat("\n\nData types:")
        for (i in seq_along(df)) {
          cat("\n  ", names(df)[i], ":", class(df[[i]])[1])
        }
        
        # First few rows
        cat("\n\nFirst 5 rows:")
        print(head(df, 5))
        
        # Date column analysis
        date_columns <- names(df)[sapply(df, function(x) inherits(x, c("POSIXct", "POSIXt", "Date")))]
        if (length(date_columns) > 0) {
          cat("\n\nDate ranges:")
          for (col in date_columns) {
            valid_dates <- df[[col]][!is.na(df[[col]])]
            if (length(valid_dates) > 0) {
              cat("\n  ", col, ":", as.character(min(valid_dates)), "to", as.character(max(valid_dates)))
            }
          }
        }
        
        # Categorical columns analysis
        char_columns <- names(df)[sapply(df, is.character)]
        if (length(char_columns) > 0) {
          cat("\n\nUnique values in character columns:")
          for (col in char_columns) {
            unique_vals <- unique(df[[col]])
            unique_vals <- unique_vals[!is.na(unique_vals)]
            cat("\n  ", col, ":", length(unique_vals), "unique values")
            if (length(unique_vals) <= 20) {
              cat("\n    Values:", paste(unique_vals, collapse = ", "))
            } else {
              cat("\n    Sample values:", paste(head(unique_vals, 10), collapse = ", "), "...")
            }
          }
        }
        
        # Numeric columns statistics
        numeric_columns <- names(df)[sapply(df, is.numeric)]
        if (length(numeric_columns) > 0) {
          cat("\n\nNumeric column statistics:")
          print(summary(df[numeric_columns]))
        }
        
        # Potential key columns (high uniqueness)
        cat("\n\nPotential key columns (high uniqueness):")
        for (col in names(df)) {
          unique_ratio <- length(unique(df[[col]])) / nrow(df)
          if (unique_ratio > 0.8) {
            cat("\n  ", col, ":", length(unique(df[[col]])), "/", nrow(df), 
                "unique (", round(unique_ratio * 100, 1), "%)")
          }
        }
        
      }, error = function(e) {
        cat("\nError reading sheet", sheet_name, ":", e$message)
      })
    }
    
    return(all_data)
    
  }, error = function(e) {
    cat("\nError reading file:", e$message)
    return(NULL)
  })
}

# Function to specifically explore crime data
explore_crime_data <- function(crime_file_path) {
  
  cat("\n", rep("#", 80), sep = "")
  cat("\nCRIME DATA SPECIFIC ANALYSIS")
  cat("\n", rep("#", 80), sep = "")
  
  crime_data <- explore_excel_file(crime_file_path, "CRIME DATA 2016-2020")
  
  if (is.null(crime_data)) {
    return(NULL)
  }
  
  # Find the main crime sheet (largest one)
  main_sheet <- names(crime_data)[which.max(sapply(crime_data, nrow))]
  
  if (!is.null(main_sheet)) {
    df <- crime_data[[main_sheet]]
    cat("\n\nAnalyzing main crime sheet:", main_sheet)
    
    # Identify key columns
    date_cols <- names(df)[grepl("date|time", names(df), ignore.case = TRUE)]
    location_cols <- names(df)[grepl("division|parish|place|location|address", names(df), ignore.case = TRUE)]
    crime_cols <- names(df)[grepl("category|crime|offense|type", names(df), ignore.case = TRUE)]
    
    cat("\nPotential date columns:", paste(date_cols, collapse = ", "))
    cat("\nPotential location columns:", paste(location_cols, collapse = ", "))
    cat("\nPotential crime type columns:", paste(crime_cols, collapse = ", "))
    
    # Division analysis
    division_col <- names(df)[grepl("division", names(df), ignore.case = TRUE)][1]
    if (!is.na(division_col)) {
      cat("\n\nUnique divisions in", division_col, ":")
      divisions <- table(df[[division_col]], useNA = "ifany")
      print(divisions)
      
      # Data quality checks
      cat("\n\nData quality checks for", division_col, ":")
      cat("\n  Null values:", sum(is.na(df[[division_col]])))
      cat("\n  Empty strings:", sum(df[[division_col]] == "", na.rm = TRUE))
      cat("\n  Whitespace issues:", length(unique(str_trim(df[[division_col]]))), "vs", 
          length(unique(df[[division_col]])), "unique values")
    }
    
    # Category analysis
    category_col <- names(df)[grepl("category", names(df), ignore.case = TRUE)][1]
    if (!is.na(category_col)) {
      cat("\n\nCrime categories in", category_col, ":")
      categories <- table(df[[category_col]], useNA = "ifany")
      print(categories)
    }
    
    # Time pattern analysis
    if (length(date_cols) > 0) {
      for (date_col in date_cols) {
        tryCatch({
          # Convert to date if not already
          if (!inherits(df[[date_col]], c("POSIXct", "POSIXt", "Date"))) {
            df[[date_col]] <- as.Date(df[[date_col]])
          }
          
          valid_dates <- df[[date_col]][!is.na(df[[date_col]])]
          
          if (length(valid_dates) > 0) {
            cat("\n\nTime patterns for", date_col, ":")
            cat("\n  Date range:", as.character(min(valid_dates)), "to", as.character(max(valid_dates)))
            cat("\n  Years covered:", paste(sort(unique(year(valid_dates))), collapse = ", "))
            
            # Records per year
            yearly_counts <- table(year(valid_dates))
            cat("\n  Records per year:")
            for (i in seq_along(yearly_counts)) {
              cat("\n    ", names(yearly_counts)[i], ":", yearly_counts[i], "records")
            }
          }
          
        }, error = function(e) {
          cat("\nCould not analyze date column", date_col, ":", e$message)
        })
      }
    }
  }
  
  return(crime_data)
}

# Function to specifically explore SOE/ZOSO data
explore_soe_data <- function(soe_file_path) {
  
  cat("\n", rep("#", 80), sep = "")
  cat("\nSOE/ZOSO DATA SPECIFIC ANALYSIS")
  cat("\n", rep("#", 80), sep = "")
  
  soe_data <- explore_excel_file(soe_file_path, "SOE AND ZOSO DATA")
  
  if (is.null(soe_data)) {
    return(NULL)
  }
  
  # Analyze SOE sheet
  if ("SOE" %in% names(soe_data)) {
    soe_df <- soe_data[["SOE"]]
    cat("\n\nSOE INTERVENTIONS ANALYSIS:")
    cat("\nTotal SOE records:", nrow(soe_df))
    
    # Convert date columns
    date_cols <- names(soe_df)[grepl("date", names(soe_df), ignore.case = TRUE)]
    for (col in date_cols) {
      if (!inherits(soe_df[[col]], c("POSIXct", "POSIXt", "Date"))) {
        soe_df[[col]] <- as.Date(soe_df[[col]])
      }
    }
    
    # Division analysis
    if ("Division" %in% names(soe_df)) {
      cat("\n\nDivisions with SOE:")
      division_counts <- table(soe_df$Division)
      print(division_counts)
    }
    
    # Timeline analysis
    if ("SOE_Effective_Date" %in% names(soe_df)) {
      effective_dates <- soe_df$SOE_Effective_Date[!is.na(soe_df$SOE_Effective_Date)]
      cat("\n\nSOE Timeline:")
      cat("\n  First SOE:", as.character(min(effective_dates)))
      cat("\n  Last SOE:", as.character(max(effective_dates)))
      cat("\n  SOEs by year:")
      yearly_soe <- table(year(effective_dates))
      for (i in seq_along(yearly_soe)) {
        cat("\n    ", names(yearly_soe)[i], ":", yearly_soe[i], "SOEs")
      }
    }
  }
  
  # Analyze ZOSO sheet
  if ("ZOSO" %in% names(soe_data)) {
    zoso_df <- soe_data[["ZOSO"]]
    cat("\n\nZOSO INTERVENTIONS ANALYSIS:")
    cat("\nTotal ZOSO records:", nrow(zoso_df))
    
    # Convert date columns
    date_cols <- names(zoso_df)[grepl("date", names(zoso_df), ignore.case = TRUE)]
    for (col in date_cols) {
      if (!inherits(zoso_df[[col]], c("POSIXct", "POSIXt", "Date"))) {
        zoso_df[[col]] <- as.Date(zoso_df[[col]])
      }
    }
    
    # Division analysis
    if ("Division" %in% names(zoso_df)) {
      cat("\n\nDivisions with ZOSO:")
      division_counts <- table(zoso_df$Division)
      print(division_counts)
    }
    
    # Timeline analysis
    if ("ZOSO_Effective_Date" %in% names(zoso_df)) {
      effective_dates <- zoso_df$ZOSO_Effective_Date[!is.na(zoso_df$ZOSO_Effective_Date)]
      cat("\n\nZOSO Timeline:")
      cat("\n  First ZOSO:", as.character(min(effective_dates)))
      cat("\n  Last ZOSO:", as.character(max(effective_dates)))
      cat("\n  ZOSOs by year:")
      yearly_zoso <- table(year(effective_dates))
      for (i in seq_along(yearly_zoso)) {
        cat("\n    ", names(yearly_zoso)[i], ":", yearly_zoso[i], "ZOSOs")
      }
    }
  }
  
  return(soe_data)
}

# Function to create data integration summary
create_data_summary <- function(crime_data, soe_data) {
  
  cat("\n", rep("*", 80), sep = "")
  cat("\nDATA INTEGRATION SUMMARY")
  cat("\n", rep("*", 80), sep = "")
  
  # Get division lists from both datasets
  crime_divisions <- character(0)
  soe_divisions <- character(0)
  
  if (!is.null(crime_data)) {
    main_crime_sheet <- names(crime_data)[which.max(sapply(crime_data, nrow))]
    crime_df <- crime_data[[main_crime_sheet]]
    
    division_col <- names(crime_df)[grepl("division", names(crime_df), ignore.case = TRUE)][1]
    if (!is.na(division_col)) {
      crime_divisions <- unique(crime_df[[division_col]][!is.na(crime_df[[division_col]])])
    }
  }
  
  if (!is.null(soe_data)) {
    if ("SOE" %in% names(soe_data)) {
      soe_divisions <- c(soe_divisions, unique(soe_data[["SOE"]]$Division[!is.na(soe_data[["SOE"]]$Division)]))
    }
    if ("ZOSO" %in% names(soe_data)) {
      soe_divisions <- c(soe_divisions, unique(soe_data[["ZOSO"]]$Division[!is.na(soe_data[["ZOSO"]]$Division)]))
    }
    soe_divisions <- unique(soe_divisions)
  }
  
  cat("\n\nDIVISION MATCHING ANALYSIS:")
  cat("\nCrime data divisions:", length(crime_divisions))
  cat("\nSOE/ZOSO divisions:", length(soe_divisions))
  
  if (length(crime_divisions) > 0 && length(soe_divisions) > 0) {
    matching <- intersect(crime_divisions, soe_divisions)
    crime_only <- setdiff(crime_divisions, soe_divisions)
    soe_only <- setdiff(soe_divisions, crime_divisions)
    
    cat("\nMatching divisions:", length(matching))
    cat("\nCrime-only divisions:", length(crime_only))
    cat("\nSOE/ZOSO-only divisions:", length(soe_only))
    
    if (length(matching) > 0) {
      cat("\n\nMatching divisions:", paste(sort(matching), collapse = ", "))
    }
    if (length(crime_only) > 0) {
      cat("\n\nCrime-only divisions:", paste(sort(crime_only), collapse = ", "))
    }
    if (length(soe_only) > 0) {
      cat("\n\nSOE/ZOSO-only divisions:", paste(sort(soe_only), collapse = ", "))
    }
  }
  
  cat("\n\nRECOMMENDATIONS FOR PANEL CONSTRUCTION:")
  cat("\n1. Use matching divisions for main analysis")
  cat("\n2. Check for spelling/naming inconsistencies in division names")
  cat("\n3. Consider whether to include non-matching divisions")
  cat("\n4. Verify date ranges overlap between datasets")
  
  # Return summary for further use
  return(list(
    crime_divisions = crime_divisions,
    soe_divisions = soe_divisions,
    matching_divisions = if(length(crime_divisions) > 0 && length(soe_divisions) > 0) intersect(crime_divisions, soe_divisions) else character(0)
  ))
}

# Main execution function
main_exploration <- function() {
  
  cat("COMPREHENSIVE DATA EXPLORATION")
  cat("\n", rep("=", 80), sep = "")
  
  # Explore both files
  crime_data <- explore_crime_data(crime_file)
  soe_data <- explore_soe_data(soe_file)
  
  # Create integration summary
  summary_info <- create_data_summary(crime_data, soe_data)
  
  # Return all data for further analysis
  return(list(
    crime_data = crime_data,
    soe_data = soe_data,
    summary = summary_info
  ))
}

# Run the exploration
cat("Starting data exploration...\n")
exploration_results <- main_exploration()

# Save the results for later use
crime_data <- exploration_results$crime_data
soe_data <- exploration_results$soe_data
division_summary <- exploration_results$summary

cat("\n\nExploration complete! Data objects created:")
cat("\n- crime_data: List containing all crime data sheets")
cat("\n- soe_data: List containing all SOE/ZOSO data sheets") 
cat("\n- division_summary: Summary of division matching between datasets")
