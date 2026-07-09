# =============================================================================
# COMPREHENSIVE GOODNESS-OF-FIT TESTING FOR LOGISTIC REGRESSION
# =============================================================================
# Author: [Your Name]
# Purpose: Apply multiple goodness-of-fit tests to 1997 birth weight data
# Features: CSV caching, customizable test names, academic visualization
# =============================================================================

# =============================================================================
# *** REQUIRED LIBRARIES ***
# =============================================================================
library(givitiR) #for givitiR Calibration test
library(ResourceSelection)  # For hoslem.test
library(lmtest) 
library(statmod) #for score test
library(dplyr)

# Load additional required packages for various tests
required_packages <- c("largesamplehl", "rms", "ggplot2", "tidyr", "BAGofT", "randomForest", "dcov", "pbapply", "MASS")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Package", pkg, "not available. Some functions may not work."))
  }
}


# =============================================================================
# *** CSV RESULTS MANAGEMENT SYSTEM ***
# =============================================================================
# CSV file path for storing results (simple format: Test_Name, P_Value)
csv_file_path <- "C:/Users/ebrah/.cursor-tutor/Proposal Tex/goodness_of_fit_results(with interaction).csv"

# Function to load existing results from CSV
load_existing_results <- function(file_path) {
  if (file.exists(file_path)) {
    tryCatch({
      existing_data <- read.csv(file_path, stringsAsFactors = FALSE)
      # Convert to named list for easy lookup
      existing_list <- as.list(existing_data$P_Value)
      names(existing_list) <- existing_data$Test_Name
      cat("✅ Loaded existing results from CSV file\n")
      cat("📊 Found", length(existing_list), "previously computed test results\n")
      return(existing_list)
    }, error = function(e) {
      cat("⚠️ Error reading CSV file, starting fresh\n")
      return(list())
    })
  } else {
    cat("📄 CSV file not found, will create new results file\n")
    return(list())
  }
}

# Function to save results to CSV
save_results_to_csv <- function(test_results, file_path) {
  # Convert list to data frame
  results_df <- data.frame(
    Test_Name = names(test_results),
    P_Value = as.numeric(test_results),
    stringsAsFactors = FALSE
  )
  
  # Write to CSV
  write.csv(results_df, file_path, row.names = FALSE)
  cat("💾 Results saved to CSV file:", file_path, "\n")
  cat("📊 Saved", nrow(results_df), "test results\n")
}


# =============================================================================
# *** CUSTOMIZABLE TEST NAME MAPPING SYSTEM ***
# =============================================================================
# You can easily modify the display names of tests here
test_name_map <- list(
  # GIVITI Tests
  "giviti_external" = "GIVITI Calibration (External)",
  "giviti_internal" = "GIVITI Calibration (Internal)",
  
  # Hosmer-Lemeshow Variants
  "traditional_HL" = "Hosmer-Lemeshow (C) (Equal n)",

  "hosmer_equal_width" = "Hosmer-Lemeshow (H) (Equal Width)",
  "large_sample_HL" = "Large Sample Hosmer-Lemeshow",
  
  # Residual Tests
  "pearson" = "Pearson Chi-Square",
  "deviance" = "Deviance Test",
  "pearson_grouped" = "Pearson Chi-Square (grouped)",
  "deviance_grouped" = "Deviance Test (grouped)",
  
  # GOF.GLM Package Tests
  "hl_ftest" = "Hosmer-Lemeshow F-Test",
  "osius_rojek" = "Osius-Rojek Test",
  "stukel_geq0.5" = "Stukel Test (≥ 0.5)",
  "stukel_l0.5" = "Stukel Test (< 0.5)", 
  "stukel_both" = "Stukel Combined Test",
  
  # Spiegelhalter & Related
  "spiegelhalter" = "Spiegelhalter Z-Test",
  "unreliability_index" = "Unreliability Index (U)",
  
  # Specialized Tests
  "copas_unweighted" = "Copas Unweighted S",
  "pigeon_heyse" = "Pigeon-Heyse Test",
  "xie_test" = "Xie Goodness-of-Fit Test",
  "eHL" = "E-Hosmer-Lemeshow (eHL)",
  "mccullagh_test" = "McCullagh Test",
  "mccullagh_test_grouped" = "McCullagh Test (grouped)",
  "lecessie_test" = "le Cessie-van Houwelingen Test",
  
  # Farrington Tests
  "farrington_standalone" = "Farrington Test (grouped)",

  
  # Ebrahim-Farrington Tests (from GitHub package)
  "ebrahim_farrington_G10" = "**Ebrahim-Farrington Test (G=10)", 
  
  # Information Matrix
  "IM_efficient" = "Information Matrix Test",
  
  # GAM-based Tests
  "HL_GAM" = "Hosmer-Lemeshow + GAM",
  "PR_GAM" = "Pulkstenis-Robinson + GAM", 
  "XIE_GAM" = "Xie + GAM",
  
  # Additional Tests
  "PR_test" = "Pulkstenis-Robinson Test",
  "stute_zhu" = "Stute-Zhu Bootstrap Test",
  "tsiatis_clustering" = "Tsiatis Clustering Test",
  
  # Projection Test
  "projection_test" = "Projection-Based Test (2024)",
  
  # BaGofT Tests
  "bagoft_split1_sim0" = "BaGofT (Split=1, Sim=0)",
  "bagoft_split20_sim0" = "BaGofT (Split=20, Sim=0)",
  "bagoft_split20_sim100" = "BaGofT (Split=20, Sim=100)"
)

# Function to apply custom names
apply_custom_names <- function(test_names) {
  # Apply custom mapping where available, keep original name otherwise
  mapped_names <- sapply(test_names, function(name) {
    if (name %in% names(test_name_map)) {
      return(test_name_map[[name]])
    } else {
      # If no custom mapping, do basic cleanup
      clean_name <- gsub("_", " ", name)
      clean_name <- gsub("test", "Test", clean_name)
      clean_name <- tools::toTitleCase(clean_name)
      return(clean_name)
    }
  })
  return(as.character(mapped_names))
}


# =============================================================================
# *** UTILITY FUNCTIONS ***
# =============================================================================
# Function to check if test already exists
test_exists <- function(test_name, existing_results) {
  return(test_name %in% names(existing_results))
}

# Load existing results
existing_results <- load_existing_results(csv_file_path)


# =============================================================================
# *** SOURCE EXTERNAL TEST FUNCTIONS ***
# =============================================================================
# Source files needed for testing (adjust paths as needed)
source_files <- c(
  "c:\\Users\\ebrah\\.cursor-tutor\\projects\\LogisticDxWithoutSukel.r", #for MHL, HL-ftest, Osius-Rojek, Stukels-st, sst_both
  "c:\\Users\\ebrah\\.cursor-tutor\\projects\\pigeonheyse.r", #for pigeon_heyse_test   
  "c:\\Users\\ebrah\\.cursor-tutor\\projects\\Xie.R", #for XieGoodnessOfFitTest
  "c:\\Users\\ebrah\\.cursor-tutor\\projects\\eHL.R", #for eHL 
  "c:\\Users\\ebrah\\.cursor-tutor\\projects\\GAM.R", #for HL_GAM, PR_GAM, XIE_GAM
  "c:\\Users\\ebrah\\.cursor-tutor\\projects\\Hosmer (H) (equal width interval).R", #for hosmer_lemeshow_equal_intervals
  "c:\\Users\\ebrah\\.cursor-tutor\\projects\\IM (infromation Matrix).R", #for IMtest_fast
  "c:\\Users\\ebrah\\.cursor-tutor\\projects\\Farrington Test.R", #for farrington_test
  "c:\\Users\\ebrah\\.cursor-tutor\\projects\\PR_test_only.R", #for PR_test
  "c:\\Users\\ebrah\\.cursor-tutor\\projects\\Stute-Zhu(Bootstrap).R", #for tsz_test
  "c:\\Users\\ebrah\\.cursor-tutor\\projects\\Tsiatis.R", #for score_gof_clustering
  "c:\\Users\\ebrah\\.cursor-tutor\\projects\\McCullaph - CPU .R", #for mccullagh_test
  "c:\\Users\\ebrah\\.cursor-tutor\\projects\\lecessie1995.r", #for leCessie.test
  "c:\\Users\\ebrah\\.cursor-tutor\\projects\\Projection_Bootstrap2024_GPU.R" #for projection_test
)

# Source the files with error handling
for (file in source_files) {
  tryCatch({
    source(file)
  }, error = function(e) {
    message(paste("Could not source", basename(file), ":", e$message))
  })
}




# =============================================================================
# *** DATA LOADING AND MODEL FITTING ***
# =============================================================================
## Load and fit the model (Ungrouped Data)
birthwgt <- read.csv("C:/Users/ebrah/.cursor-tutor/projects/SA_Data.csv")
model_grouped <- glm(y ~ age + lwt + race2 + race3 + smoke + age:lwt + smoke:lwt, data = birthwgt, family = binomial())
predicted_probs_grouped <- fitted(model_grouped)

# Load and fit the model (Grouped Data)
# birthwgt <- read.csv("C:/Users/ebrah/.cursor-tutor/projects/Data SAS(Grouped).csv")
# model_grouped <- glm(y~ age + lwt + race2 + race3 + smoke  , data = birthwgt, family = binomial() , weights = trials)
# predicted_probs_grouped <- fitted(model_grouped)

# Create data frame for convenience
dat <- data.frame(
  y = birthwgt$y,
  age = birthwgt$age,
  lwt = birthwgt$lwt,
  race2 = birthwgt$race2,
  race3 = birthwgt$race3,
  smoke = birthwgt$smoke,
  age_lwt = birthwgt$age * birthwgt$lwt,
  smoke_lwt = birthwgt$smoke * birthwgt$lwt
)

# Display model summary
cat("==============================================\n")
cat("FITTED MODEL SUMMARY\n")
cat("==============================================\n")
print(summary(model_grouped))

cat("\n==============================================\n")
cat("GOODNESS-OF-FIT TEST RESULTS\n")
cat("==============================================\n")

# Initialize results vector
test_results <- list()

# Load existing results into test_results if available
if (!is.null(existing_results) && length(existing_results) > 0) {
  for (test_name in names(existing_results)) {
    p_value <- existing_results[[test_name]]
    test_results[[test_name]] <- p_value
    cat("📋 Loaded existing result -", test_name, ":", sprintf("%.6f", p_value), "\n")
  }
  cat("\n")
}

# =============================================================================
# *** GOODNESS-OF-FIT TESTING EXECUTION ***
# =============================================================================

# 1. GIVITI Calibration Belt Tests
cat("\n--- CALIBRATION TESTS ---\n")
if (!test_exists("giviti_external", existing_results)) {
  tryCatch({
    giviti_external <- givitiCalibrationBelt(o = birthwgt$y, e = predicted_probs_grouped, devel = "external")
    test_results$giviti_external <- giviti_external$p.value
    cat("✅ GIVITI Calibration (external):", sprintf("%.6f", giviti_external$p.value), "\n")
  }, error = function(e) { cat("❌ GIVITI Calibration (external): Error -", e$message, "\n") })
} else {
  cat("⏭️ GIVITI Calibration (external): Already computed\n")
}

if (!test_exists("giviti_internal", existing_results)) {
  tryCatch({
    giviti_internal <- givitiCalibrationBelt(o = birthwgt$y, e = predicted_probs_grouped, devel = "internal")
    test_results$giviti_internal <- giviti_internal$p.value
    cat("✅ GIVITI Calibration (internal):", sprintf("%.6f", giviti_internal$p.value), "\n")
  }, error = function(e) { cat("❌ GIVITI Calibration (internal): Error -", e$message, "\n") })
} else {
  cat("⏭️ GIVITI Calibration (internal): Already computed\n")
}


# =============================================================================
# *** HOSMER-LEMESHOW FAMILY TESTS ***
# =============================================================================
# 2. Hosmer-Lemeshow Variants
cat("\n--- HOSMER-LEMESHOW VARIANTS ---\n")
if (!test_exists("traditional_HL", existing_results)) {
  tryCatch({
    traditional_hl <- hoslem.test(birthwgt$y, predicted_probs_grouped)
    test_results$traditional_HL <- traditional_hl$p.value
    cat("✅ Traditional Hosmer-Lemeshow:", sprintf("%.6f", traditional_hl$p.value), "\n")
  }, error = function(e) { cat("❌ Traditional Hosmer-Lemeshow: Error -", e$message, "\n") })
} else {
  cat("⏭️ Traditional Hosmer-Lemeshow: Already computed\n")
}

if (!test_exists("hosmer_equal_width", existing_results)) {
  tryCatch({
    if (exists("hosmer_lemeshow_equal_intervals")) {
      hosmer_eq_width <- hosmer_lemeshow_equal_intervals(predicted_probs_grouped, birthwgt$y)
      test_results$hosmer_equal_width <- hosmer_eq_width$p.value
      cat("✅ Hosmer-Lemeshow Equal Width:", sprintf("%.6f", hosmer_eq_width$p.value), "\n")
    }
  }, error = function(e) { cat("❌ Hosmer-Lemeshow Equal Width: Error -", e$message, "\n") })
} else {
  cat("⏭️ Hosmer-Lemeshow Equal Width: Already computed\n")
}

if (!test_exists("large_sample_HL", existing_results)) {
  tryCatch({
    if (exists("hltest")) {
      large_hl <- hltest(model_grouped)
      test_results$large_sample_HL <- large_hl$p.value
      cat("✅ Large Sample Hosmer-Lemeshow:", sprintf("%.6f", large_hl$p.value), "\n")
    }
  }, error = function(e) { cat("❌ Large Sample Hosmer-Lemeshow: Error -", e$message, "\n") })
} else {
  cat("⏭️ Large Sample Hosmer-Lemeshow: Already computed\n")
}

# =============================================================================
# *** CLASSICAL RESIDUAL-BASED TESTS ***
# =============================================================================
# 3. Residual-Based Tests
cat("\n--- RESIDUAL-BASED TESTS ---\n")
if (!test_exists("pearson", existing_results)) {
  tryCatch({
    pearson_residuals <- residuals(model_grouped, type = "pearson")
    pearson_chi_square <- sum(pearson_residuals^2)
    pearson_p <- 1 - pchisq(pearson_chi_square, df = model_grouped$df.residual)
    test_results$pearson <- pearson_p
    cat("✅ Pearson Chi-Square:", sprintf("%.6f", pearson_p), "\n")
  }, error = function(e) { cat("❌ Pearson Chi-Square: Error -", e$message, "\n") })
} else {
  cat("⏭️ Pearson Chi-Square: Already computed\n")
}

if (!test_exists("deviance", existing_results)) {
  tryCatch({
    predicted_probs_adj <- pmin(pmax(predicted_probs_grouped, 1e-8), 1 - 1e-8)
    y_adj <- pmin(pmax(birthwgt$y, 1e-8), 1 - 1e-8)
    D <- -2 * sum(
      birthwgt$y * log(predicted_probs_adj / y_adj) +
        (1 - birthwgt$y) * log((1 - predicted_probs_adj) / (1 - y_adj))
    )
    deviance_p <- pchisq(D, model_grouped$df.residual, lower.tail = FALSE)
    test_results$deviance <- deviance_p
    cat("✅ Deviance Test:", sprintf("%.6f", deviance_p), "\n")
  }, error = function(e) { cat("❌ Deviance Test: Error -", e$message, "\n") })
} else {
  cat("⏭️ Deviance Test: Already computed\n")
}

# =============================================================================
# *** grouped DATA TESTS (COVARIATE PATTERN ANALYSIS) ***
# =============================================================================
# 3b.(Grouped) Data Tests using Covariate Patterns
cat("\n--- grouped DATA TESTS (COVARIATE PATTERNS) ---\n")
if (!test_exists("pearson_grouped", existing_results)) {
  tryCatch({
    # Load grouped data for grouped tests
    birthwgt_trials <- read.csv("C:/Users/ebrah/.cursor-tutor/projects/Data SAS(Grouped).csv")
    model_grouped_trials <- glm(cbind(y, trials - y) ~ age + lwt + race2 + race3 + smoke + age:lwt + smoke:lwt, 
                               data = birthwgt_trials, family = binomial())
    predicted_probs_grouped_trials <- fitted(model_grouped_trials)
    
    # Number of covariate patterns
    G <- nrow(birthwgt_trials)
    p <- length(coef(model_grouped_trials)) - 1  # Number of parameters minus intercept
    
    # Pearson Chi-Square for grouped data (Equation from thesis)
    # X² = Σ(y_g - m_g*π̂_g)² / (m_g*π̂_g*(1-π̂_g))
    y_g <- birthwgt_trials$y
    m_g <- birthwgt_trials$trials
    pi_g <- predicted_probs_grouped_trials
    
    pearson_grouped_stat <- sum((y_g - m_g * pi_g)^2 / (m_g * pi_g * (1 - pi_g)))
    pearson_grouped_df <- G - (p + 1)  # G - (p+1) degrees of freedom
    pearson_grouped_p <- 1 - pchisq(pearson_grouped_stat, df = pearson_grouped_df)
    
    test_results$pearson_grouped <- pearson_grouped_p
    cat("✅ Pearson Chi-Square (grouped):", sprintf("%.6f", pearson_grouped_p), "\n")
    cat("    Statistic:", sprintf("%.4f", pearson_grouped_stat), "on", pearson_grouped_df, "df\n")
    
  }, error = function(e) { cat("❌ Pearson Chi-Square (grouped): Error -", e$message, "\n") })
} else {
  cat("⏭️ Pearson Chi-Square (grouped): Already computed\n")
}

if (!test_exists("deviance_grouped", existing_results)) {
  tryCatch({
    # Load grouped data if not already loaded
    if (!exists("birthwgt_trials")) {
      birthwgt_trials <- read.csv("C:/Users/ebrah/.cursor-tutor/projects/Data SAS(Grouped).csv")
      model_grouped_trials <- glm(cbind(y, trials - y) ~ age + lwt + race2 + race3 + smoke + age:lwt + smoke:lwt, 
                                 data = birthwgt_trials, family = binomial())
      predicted_probs_grouped_trials <- fitted(model_grouped_trials)
      
      G <- nrow(birthwgt_trials)
      p <- length(coef(model_grouped_trials)) - 1
      y_g <- birthwgt_trials$y
      m_g <- birthwgt_trials$trials
      pi_g <- predicted_probs_grouped_trials
    }
    
    # Deviance for grouped data (Equation from thesis)
    # D = 2*Σ[y_g*log(y_g/(m_g*π̂_g)) + (m_g-y_g)*log((m_g-y_g)/(m_g*(1-π̂_g)))]
    
    # Handle zero cases to avoid log(0)
    y_g_safe <- pmax(y_g, 1e-10)
    mg_minus_yg_safe <- pmax(m_g - y_g, 1e-10)
    
    deviance_grouped_stat <- 2 * sum(
      y_g * log(y_g_safe / (m_g * pi_g)) + 
      (m_g - y_g) * log(mg_minus_yg_safe / (m_g * (1 - pi_g)))
    )
    
    deviance_grouped_df <- G - (p + 1)  # G - (p+1) degrees of freedom
    deviance_grouped_p <- 1 - pchisq(deviance_grouped_stat, df = deviance_grouped_df)
    
    test_results$deviance_grouped <- deviance_grouped_p
    cat("✅ Deviance Test (grouped):", sprintf("%.6f", deviance_grouped_p), "\n")
    cat("    Statistic:", sprintf("%.4f", deviance_grouped_stat), "on", deviance_grouped_df, "df\n")
    
  }, error = function(e) { cat("❌ Deviance Test (grouped): Error -", e$message, "\n") })
} else {
  cat("⏭️ Deviance Test (grouped): Already computed\n")
}


# =============================================================================
# *** SPECIALIZED GOODNESS-OF-FIT TESTS ***
# =============================================================================

if (!test_exists("tsiatis_clustering", existing_results)) {
  tryCatch({
    if (exists("score_gof_clustering")) {
      num_groups <- max(4, min(10, floor(sqrt(nrow(dat))/2)))
      tsiatis_cluster <- score_gof_clustering(model_grouped, num_groups = num_groups, y = birthwgt$y)
      test_results$tsiatis_clustering <- tsiatis_cluster$p_value
      cat("✅ Tsiatis Clustering Test:", sprintf("%.6f", tsiatis_cluster$p_value), "\n")
    }
  }, error = function(e) { cat("❌ Tsiatis Clustering Test: Error -", e$message, "\n") })
} else {
  cat("⏭️ Tsiatis Clustering Test: Already computed\n")
}


# =============================================================================
# *** GOF.GLM PACKAGE TESTS ***
# =============================================================================
# 5. GOF.GLM Package Tests
cat("\n--- GOF.GLM PACKAGE TESTS ---\n")
if (!test_exists("osius_rojek", existing_results) || !test_exists("stukel_geq0.5", existing_results) || 
    !test_exists("stukel_l0.5", existing_results) || !test_exists("stukel_both", existing_results)) {
  tryCatch({
    if (exists("gof.glm")) {
      gof_results <- suppressWarnings(gof.glm(model_grouped, g=8, plotROC = FALSE))
      if (!is.null(gof_results) && !is.null(gof_results$gof) && !is.null(gof_results$gof$pVal)) {
        #test_results$hl_ftest <- gof_results$gof$pVal[2]
        test_results$osius_rojek <- gof_results$gof$pVal[3]
        test_results$stukel_geq0.5 <- gof_results$gof$pVal[4]
        test_results$stukel_l0.5 <- gof_results$gof$pVal[5]
        test_results$stukel_both <- gof_results$gof$pVal[6]
        
        #cat("HL F-test:", sprintf("%.6f", gof_results$gof$pVal[2]), "\n")
        cat("✅ Osius-Rojek Test:", sprintf("%.6f", gof_results$gof$pVal[3]), "\n")
        cat("✅ Stukel Test (≥0.5):", sprintf("%.6f", gof_results$gof$pVal[4]), "\n")
        cat("✅ Stukel Test (<0.5):", sprintf("%.6f", gof_results$gof$pVal[5]), "\n")
        cat("✅ Stukel Both Test:", sprintf("%.6f", gof_results$gof$pVal[6]), "\n")
      }
    }
  }, error = function(e) { cat("❌ GOF.GLM tests: Error -", e$message, "\n") })
} else {
  cat("⏭️ GOF.GLM tests: Already computed\n")
}


# =============================================================================
# *** SPIEGELHALTER & CALIBRATION TESTS ***
# =============================================================================
# 6. Spiegelhalter Tests
cat("\n--- SPIEGELHALTER & RELATED TESTS ---\n")
if (!test_exists("spiegelhalter", existing_results) || !test_exists("unreliability_index", existing_results)) {
  tryCatch({
    if (exists("val.prob")) {
      spg_result <- val.prob(predicted_probs_grouped, birthwgt$y, pl = FALSE, g=10)
      if (length(spg_result) >= 18 && !is.null(spg_result[[18]])) {
        test_results$spiegelhalter <- spg_result[[18]][1]
        cat("✅ Spiegelhalter z-test:", sprintf("%.6f", spg_result[[18]][1]), "\n")
      }
      if (!is.null(spg_result["U:p"])) {
        test_results$unreliability_index <- as.numeric(spg_result["U:p"])
        cat("✅ Unreliability Index (U):", sprintf("%.6f", as.numeric(spg_result["U:p"])), "\n")
      }
    }
  }, error = function(e) { cat("❌ Spiegelhalter tests: Error -", e$message, "\n") })
} else {
  cat("⏭️ Spiegelhalter tests: Already computed\n")
}


# =============================================================================
# *** ADVANCED SPECIALIZED TESTS ***
# =============================================================================
# 7. Specialized Tests
cat("\n--- SPECIALIZED TESTS ---\n")
if (!test_exists("copas_unweighted", existing_results)) {
  tryCatch({
    if (exists("lrm") && exists("resid")) {
      copas_result <- resid(lrm(y ~ age + lwt + race2 + race3 + smoke + age:lwt + smoke:lwt, data = dat, x=TRUE, y=TRUE), "gof")
      if (length(copas_result) >= 5) {
        test_results$copas_unweighted <- copas_result[5]
        cat("✅ Copas Unweighted S:", sprintf("%.6f", copas_result[5]), "\n")
      }
    }
  }, error = function(e) { cat("❌ Copas Unweighted S: Error -", e$message, "\n") })
} else {
  cat("⏭️ Copas Unweighted S: Already computed\n")
}

if (!test_exists("pigeon_heyse", existing_results)) {
  tryCatch({
    if (exists("pigeon_heyse_test")) {
      ph_result <- pigeon_heyse_test(dat, model_grouped)
      test_results$pigeon_heyse <- ph_result$p_value
      cat("✅ Pigeon-Heyse Test:", sprintf("%.6f", ph_result$p_value), "\n")
    }
  }, error = function(e) { cat("❌ Pigeon-Heyse Test: Error -", e$message, "\n") })
} else {
  cat("⏭️ Pigeon-Heyse Test: Already computed\n")
}

if (!test_exists("xie_test", existing_results)) {
  tryCatch({
    if (exists("XieGoodnessOfFitTest")) {
      xie_result <- XieGoodnessOfFitTest(dat, list(predicted_probs = predicted_probs_grouped))
      test_results$xie_test <- xie_result
      cat("✅ Xie Test:", sprintf("%.6f", xie_result), "\n")
    }
  }, error = function(e) { cat("❌ Xie Test: Error -", e$message, "\n") })
} else {
  cat("⏭️ Xie Test: Already computed\n")
}

if (!test_exists("eHL", existing_results)) {
  tryCatch({
    if (exists("eHL")) {
      ehl_result <- eHL(birthwgt$y, predicted_probs_grouped, boot=10, s=0.5)
      evalue <- ehl_result$HLe
      pvalue <- min(1, 1/evalue)
      test_results$eHL <- pvalue
      cat("✅ Extended Hosmer-Lemeshow:", sprintf("%.6f", pvalue), "\n")
    }
  }, error = function(e) { cat("❌ Extended Hosmer-Lemeshow: Error -", e$message, "\n") })
} else {
  cat("⏭️ Extended Hosmer-Lemeshow: Already computed\n")
}

if (!test_exists("mccullagh_test", existing_results)) {
  tryCatch({
    if (exists("mccullagh_test")) {
      mccullagh_result <- mccullagh_test(birthwgt$y, predicted_probs_grouped, model_grouped, dat)
      test_results$mccullagh_test <- mccullagh_result$p_value
      cat("✅ McCullagh Test:", sprintf("%.6f", mccullagh_result$p_value), "\n")
    }
  }, error = function(e) { cat("❌ McCullagh Test: Error -", e$message, "\n") })
} else {
  cat("⏭️ McCullagh Test: Already computed\n")
}

if (!test_exists("mccullagh_test_grouped", existing_results)) {
  tryCatch({
   birthwgt_trials <- read.csv("C:/Users/ebrah/.cursor-tutor/projects/Data SAS(Grouped).csv")
   model_grouped_trials <- glm( birthwgt_trials$y/birthwgt_trials$trials ~ birthwgt_trials$age + birthwgt_trials$lwt + birthwgt_trials$race2 + birthwgt_trials$race3 + birthwgt_trials$smoke + birthwgt_trials$age:birthwgt_trials$lwt + birthwgt_trials$smoke:birthwgt_trials$lwt, data = birthwgt_trials, family = binomial(),weights = birthwgt_trials$trials)
   predicted_probs_grouped_trials <- fitted(model_grouped_trials)

  ## Call the function with `m`
   mccullagh_test_grouped <- mccullagh_test_grouped(model_grouped_trials)

   
    test_results$mccullagh_test_grouped <- mccullagh_test_grouped$p_value
    cat("✅ McCullagh Test (Grouped):", sprintf("%.6f", mccullagh_test_grouped$p_value), "\n")
  }, error = function(e) { cat("❌ McCullagh Test (Grouped): Error -", e$message, "\n") })
} else {
  cat("⏭️ McCullagh Test (Grouped): Already computed\n")
}

if (!test_exists("lecessie_test", existing_results)) {
  tryCatch({
    if (exists("leCessie.test")) {
      lecessie_result <- leCessie.test(model_grouped)
      test_results$lecessie_test <- lecessie_result$p.value
      cat("✅ le Cessie-van Houwelingen Test:", sprintf("%.6f", lecessie_result$p.value), "\n")
    }
  }, error = function(e) { cat("❌ le Cessie-van Houwelingen Test: Error -", e$message, "\n") })
} else {
  cat("⏭️ le Cessie-van Houwelingen Test: Already computed\n")
}

# =============================================================================
# *** FARRINGTON GOODNESS-OF-FIT TESTS ***
# =============================================================================
# 8. Farrington Tests
cat("\n--- FARRINGTON TESTS ---\n")
if (!test_exists("farrington_standalone", existing_results)) {
  ##ORIGINAL Farrington Test
  tryCatch({
   birthwgt_trials <- read.csv("C:/Users/ebrah/.cursor-tutor/projects/Data SAS(Grouped).csv")
   model_grouped_trials <- glm(cbind(y, trials - y) ~ age + lwt + race2 + race3 + smoke + age:lwt + smoke:lwt, data = birthwgt_trials, family = binomial())
   predicted_probs_grouped_trials <- fitted(model_grouped_trials)

  ## Call the function with `m`
   farrington_test_standalone_trial <- farrington_test(
    y = birthwgt_trials$y,
    predicted_probs = predicted_probs_grouped_trials,
    model = model_grouped_trials,
    m = birthwgt_trials$trials
   )

   
    test_results$farrington_standalone <- farrington_test_standalone_trial$p_value
    cat("✅ Farrington (standalone):", sprintf("%.6f", farrington_test_standalone_trial$p_value), "\n")
  }, error = function(e) { cat("❌ Farrington (standalone): Error -", e$message, "\n") })
} else {
  cat("⏭️ Farrington (standalone): Already computed\n")
}

# =============================================================================
# *** EBRAHIM-FARRINGTON GOODNESS-OF-FIT TESTS (GITHUB PACKAGE) ***
# =============================================================================
# New Ebrahim-Farrington Tests from GitHub package
cat("\n--- EBRAHIM-FARRINGTON TESTS (GITHUB PACKAGE) ---\n")

# First try to install/load the ebrahim.gof package
if (!test_exists("ebrahim_farrington_G10", existing_results)) {
  
  # Try to load or install the package
  package_loaded <- FALSE
  tryCatch({
    # Try to load the package first
    if (require(ebrahim.gof, quietly = TRUE)) {
      package_loaded <- TRUE
      cat("📦 ebrahim.gof package loaded successfully\n")
    } else {
      cat("📦 Installing ebrahim.gof package from GitHub...\n")
      # Install devtools if not available
      if (!require(devtools, quietly = TRUE)) {
        install.packages("devtools")
        library(devtools)
      }
      # Install the package from GitHub
      devtools::install_github("ebrahimkhaled/ebrahim.gof", quiet = TRUE)
      library(ebrahim.gof)
      package_loaded <- TRUE
      cat("✅ ebrahim.gof package installed and loaded\n")
    }
  }, error = function(e) {
    cat("❌ Could not install/load ebrahim.gof package:", e$message, "\n")
    cat("💡 Please install manually: devtools::install_github('ebrahimkhaled/ebrahim.gof')\n")
    package_loaded <- FALSE
  })
  
  # Run the tests if package is loaded
  if (package_loaded && exists("ef.gof")) {
    
    # Ebrahim-Farrington Test with G=10
    if (!test_exists("ebrahim_farrington_G10", existing_results)) {
      tryCatch({
        ef_G10 <- ef.gof(y = birthwgt$y, predicted_probs = predicted_probs_grouped, G = 10)
        test_results$ebrahim_farrington_G10 <- ef_G10$p_value
        cat("✅ Ebrahim-Farrington (G=10):", sprintf("%.6f", ef_G10$p_value), "\n")
      }, error = function(e) { cat("❌ Ebrahim-Farrington (G=10): Error -", e$message, "\n") })
    } else {
      cat("⏭️ Ebrahim-Farrington (G=10): Already computed\n")
    }
    
  } else {
    cat("⏭️ ef.gof function not available. Skipping Ebrahim-Farrington tests.\n")
  }
} else {
  cat("⏭️ Ebrahim-Farrington (G=10): Already computed\n")
}

# =============================================================================
# *** INFORMATION MATRIX GOODNESS-OF-FIT TEST ***
# =============================================================================
# 9. Information Matrix Test
cat("\n--- INFORMATION MATRIX TEST ---\n")
if (!test_exists("IM_efficient", existing_results)) {
  tryCatch({
    if (exists("IMtest_fast")) {
      im_result <- IMtest_fast(predicted_probs = predicted_probs_grouped, model = model_grouped, data = dat)
      test_results$IM_efficient <- im_result$p.value
      cat("✅ IM Test (Efficient):", sprintf("%.6f", im_result$p.value), "\n")
    }
  }, error = function(e) { cat("❌ IM Test (Efficient): Error -", e$message, "\n") })
} else {
  cat("⏭️ IM Test (Efficient): Already computed\n")
}

# =============================================================================
# *** GAM-ENHANCED GOODNESS-OF-FIT TESTS ***
# =============================================================================
# 10. GAM-based Tests
cat("\n--- GAM-BASED TESTS ---\n")
if (!test_exists("HL_GAM", existing_results) || !test_exists("PR_GAM", existing_results) || !test_exists("XIE_GAM", existing_results)) {
  tryCatch({
    if (exists("gam_gof_tests")) {
      predictors <- c("age", "lwt", "race2", "race3", "smoke")
      categorical_vars <- c("race2", "race3", "smoke")
      gam_result <- gam_gof_tests(
        df = dat,
        response = "y",
        predictors = predictors,
        categorical_vars = categorical_vars,
        model_probs = predicted_probs_grouped
      )
      test_results$HL_GAM <- gam_result$HL_GAM$p.value
      test_results$PR_GAM <- gam_result$PR_GAM$p.value
      test_results$XIE_GAM <- gam_result$XIE_GAM$p.value
      
      cat("✅ HL+GAM Test:", sprintf("%.6f", gam_result$HL_GAM$p.value), "\n")
      cat("✅ PR+GAM Test:", sprintf("%.6f", gam_result$PR_GAM$p.value), "\n")
      cat("✅ XIE+GAM Test:", sprintf("%.6f", gam_result$XIE_GAM$p.value), "\n")
    }
  }, error = function(e) { cat("❌ GAM-based tests: Error -", e$message, "\n") })
} else {
  cat("⏭️ GAM-based tests: Already computed\n")
}

# =============================================================================
# *** BOOTSTRAP & PROJECTION TESTS ***
# =============================================================================
# 11. Additional Tests
cat("\n--- ADDITIONAL TESTS ---\n")
if (!test_exists("PR_test", existing_results)) {
  tryCatch({
    if (exists("pr_test")) {
      pr_result <- pr_test(
        df = dat,
        response = "y",
        categorical_vars = c("race2", "race3", "smoke"),
        fitted_probs = predicted_probs_grouped
      )
      test_results$PR_test <- pr_result$p_value
      cat("✅ PR Test:", sprintf("%.6f", pr_result$p_value), "\n")
    }
  }, error = function(e) { cat("❌ PR Test: Error -", e$message, "\n") })
} else {
  cat("⏭️ PR Test: Already computed\n")
}

if (!test_exists("stute_zhu", existing_results)) {
  tryCatch({
    if (exists("tsz_test")) {
      tsz_result <- tsz_test(model_grouped, n_boot = 200, parallel = FALSE)
      test_results$stute_zhu <- tsz_result$p_value
      cat("✅ Stute-Zhu Test:", sprintf("%.6f", tsz_result$p_value), "\n")
    }
  }, error = function(e) { cat("❌ Stute-Zhu Test: Error -", e$message, "\n") })
} else {
  cat("⏭️ Stute-Zhu Test: Already computed\n")
}


# =============================================================================
# *** MODERN PROJECTION-BASED TEST (2024) ***
# =============================================================================
# 12. Projection-Based Goodness of Fit Test
cat("\n--- PROJECTION TEST ---\n")
if (!test_exists("projection_test", existing_results)) {
  tryCatch({
    if (exists("projection_test")) {
      proj_result <- projection_test(model_grouped, n_bootstrap = 200, alpha = 0.05)
      test_results$projection_test <- proj_result$p_value
      cat("✅ Projection-Based Test:", sprintf("%.6f", proj_result$p_value), "\n")
      cat("    Interpretation:", proj_result$interpretation, "\n")
    }
  }, error = function(e) { cat("❌ Projection-Based Test: Error -", e$message, "\n") })
} else {
  cat("⏭️ Projection-Based Test: Already computed\n")
}


# =============================================================================
# *** BOOTSTRAP AGGREGATED GOODNESS-OF-FIT TESTS ***
# =============================================================================
# BaGofT Tests
cat("\n--- BAGOFT TESTS ---\n")
if (!test_exists("bagoft_split1_sim0", existing_results)) {
  tryCatch({
    if (exists("BAGofT") && exists("testGlmBi")) {
      # Add dummy variable if needed for BaGofT partitioning
      dat_bagoft <- dat
      if (ncol(dat_bagoft) == 2) {
        dat_bagoft$dummy <- 1
      }
      
      # BaGofT test with Number of Split=1, Simulation=0
      bagoft_result_1 <- BAGofT(
        testModel = testGlmBi(formula = y ~ age + lwt + race2 + race3 + smoke + age:lwt + smoke:lwt, link = "logit"),
        parFun = parRF(),
        data = dat_bagoft,
        nsplits = 1,
        nsim = 0  
      )
      test_results$bagoft_split1_sim0 <- bagoft_result_1$pmedian
      cat("✅ BaGofT (Split=1, Sim=0):", sprintf("%.6f", bagoft_result_1$pmedian), "\n")
    }
  }, error = function(e) { cat("❌ BaGofT (Split=1, Sim=0): Error -", e$message, "\n") })
} else {
  cat("⏭️ BaGofT (Split=1, Sim=0): Already computed\n")
}

if (!test_exists("bagoft_split20_sim0", existing_results)) {
  tryCatch({
    if (exists("BAGofT") && exists("testGlmBi")) {
      # Add dummy variable if needed for BaGofT partitioning
      dat_bagoft <- dat
      if (ncol(dat_bagoft) == 2) {
        dat_bagoft$dummy <- 1
      }
      
      # BaGofT test with Number of Split=20, Simulation=0
      bagoft_result_20 <- BAGofT(
        testModel = testGlmBi(formula = y ~ age + lwt + race2 + race3 + smoke + age:lwt + smoke:lwt, link = "logit"),
        parFun = parRF(),
        data = dat_bagoft,
        nsplits = 20,
        nsim = 0  
      )
      test_results$bagoft_split20_sim0 <- bagoft_result_20$pmedian
      cat("✅ BaGofT (Split=20, Sim=0):", sprintf("%.6f", bagoft_result_20$pmedian), "\n")
    }
  }, error = function(e) { cat("❌ BaGofT (Split=20, Sim=0): Error -", e$message, "\n") })
} else {
  cat("⏭️ BaGofT (Split=20, Sim=0): Already computed\n")
}

if (!test_exists("bagoft_split20_sim100", existing_results)) {
  tryCatch({
    if (exists("BAGofT") && exists("testGlmBi")) {
      # Add dummy variable if needed for BaGofT partitioning
      dat_bagoft <- dat
      if (ncol(dat_bagoft) == 2) {
        dat_bagoft$dummy <- 1
      }
      
      # BaGofT parallel test with Number of Split=20, Simulation=100
      bagoft_result_parallel <- BAGofT(
        testModel = testGlmBi(formula = y ~ age + lwt + race2 + race3 + smoke + age:lwt + smoke:lwt, link = "logit"),
        parFun = parRF(),
        data = dat_bagoft,
        nsplits = 20,
        nsim = 100  
      )
      test_results$bagoft_split20_sim100 <- bagoft_result_parallel$pmedian
      cat("✅ BaGofT (Split=20, Sim=100):", sprintf("%.6f", bagoft_result_parallel$pmedian), "\n")
    }
  }, error = function(e) { cat("❌ BaGofT (Split=20, Sim=100): Error -", e$message, "\n") })
} else {
  cat("⏭️ BaGofT (Split=20, Sim=100): Already computed\n")
}


# =============================================================================
# *** RESULTS SUMMARY & ANALYSIS ***
# =============================================================================
# Summary of results
cat("\n==============================================\n")
cat("SUMMARY OF ALL P-VALUES\n")
cat("==============================================\n")
valid_results <- test_results[!sapply(test_results, is.na)]
for (test_name in names(valid_results)) {
  cat(sprintf("%-25s: %.6f\n", test_name, valid_results[[test_name]]))
}

# Save all results to CSV
cat("\n==============================================\n")
cat("SAVING RESULTS TO CSV\n")
cat("==============================================\n")
save_results_to_csv(test_results, csv_file_path)

cat("\n==============================================\n")
cat("INTERPRETATION (α = 0.05)\n")
cat("==============================================\n")
significant_tests <- names(valid_results)[valid_results < 0.05]
if (length(significant_tests) > 0) {
  cat("Tests suggesting model inadequacy (p < 0.05):\n")
  for (test in significant_tests) {
    cat(sprintf("  - %s: %.6f\n", test, valid_results[[test]]))
  }
} else {
  cat("No tests suggest model inadequacy at α = 0.05 level.\n")
}

# Create GIVITI calibration plot
cat("\n==============================================\n")
cat("CALIBRATION PLOT\n")
cat("==============================================\n")
#plot(givitiCalibrationBelt(o = birthwgt$y, e = predicted_probs_grouped, devel = "internal"))

# =============================================================================
# *** GIVITI CALIBRATION BELT PLOT (INTERNAL VALIDATION) ***
# =============================================================================
cat("\n==============================================\n")
cat("GIVITI CALIBRATION BELT PLOT (INTERNAL)\n")
cat("==============================================\n")

tryCatch({
  # Check if givitiR package is available
  if (require(givitiR, quietly = TRUE)) {
    cat("📊 Creating GIVITI calibration belt plot (internal validation)...\n")
    
    # Perform internal calibration belt analysis
    giviti_internal_plot <- givitiCalibrationBelt(o = birthwgt$y, e = predicted_probs_grouped, devel = "internal")
    
    # Set up high-quality PNG device for the plot
    giviti_plot_path_png <- "C:/Users/ebrah/.cursor-tutor/Proposal Tex/giviti_calibration_belt_internal(with interaction).png"
    png(filename = giviti_plot_path_png, 
        width = 10, height = 8, units = "in", res = 300, bg = "white")
    
    # Create the calibration belt plot
    plot(giviti_internal_plot, main = "GIVITI Calibration Belt (Internal Validation)", 
         sub = paste("P-value:", sprintf("%.6f", giviti_internal_plot$p.value)))
    
    # Close the PNG device
    dev.off()
    
    # Also create PDF version
    giviti_plot_path_pdf <- "C:/Users/ebrah/.cursor-tutor/Proposal Tex/giviti_calibration_belt_internal(with interaction).pdf"
    pdf(file = giviti_plot_path_pdf, 
        width = 10, height = 8, bg = "white")
    
    # Recreate the same plot for PDF
    plot(giviti_internal_plot, main = "GIVITI Calibration Belt (Internal Validation)", 
         sub = paste("P-value:", sprintf("%.6f", giviti_internal_plot$p.value)))
    
    dev.off()
    
    cat("✅ GIVITI calibration belt plot (internal) saved to:\n")
    cat("PNG:", giviti_plot_path_png, "\n")
    cat("PDF:", giviti_plot_path_pdf, "\n")
    cat("📊 P-value:", sprintf("%.6f", giviti_internal_plot$p.value), "\n")
    
  } else {
    cat("⚠️ givitiR package not available. Skipping GIVITI calibration belt plot.\n")
  }
  
}, error = function(e) {
  cat("❌ Error creating GIVITI calibration belt plot:", e$message, "\n")
  # Close any open graphics devices in case of error
  if (dev.cur() != 1) dev.off()
})


# =============================================================================
# *** RMS RELIABILITY (CALIBRATION) PLOT ***
# =============================================================================
# Create reliability plot using rms package
cat("\n==============================================\n")
cat("RMS RELIABILITY PLOT\n")
cat("==============================================\n")

tryCatch({
  # Check if rms package is available
  if (require(rms, quietly = TRUE)) {
    cat("📊 Creating RMS reliability (calibration) plot...\n")
    
    # Set up high-quality PNG device for the plot
    reliability_plot_path_png <- "C:/Users/ebrah/.cursor-tutor/Proposal Tex/rms_reliability_plot(with interaction).png"
    png(filename = reliability_plot_path_png, 
        width = 10, height = 8, units = "in", res = 300, bg = "white")
    
    # Create simple reliability plot using val.prob from rms
    reliability_result <- val.prob(
      p = predicted_probs_grouped,        # Predicted probabilities
      y = birthwgt$y,                     # Observed outcomes
      pl = TRUE                           # Create plot
    )
    
    # Close the PNG device
    dev.off()
    
    # Also create PDF version
    reliability_plot_path_pdf <- "C:/Users/ebrah/.cursor-tutor/Proposal Tex/rms_reliability_plot(with interaction).pdf"
    pdf(file = reliability_plot_path_pdf, 
        width = 10, height = 8, bg = "white")
    
    # Recreate the same simple plot for PDF
    reliability_result_pdf <- val.prob(
      p = predicted_probs_grouped,
      y = birthwgt$y,
      pl = TRUE
    )
    
    dev.off()
    
    cat("✅ RMS reliability plot saved to:\n")
    cat("PNG:", reliability_plot_path_png, "\n")
    cat("PDF:", reliability_plot_path_pdf, "\n")
    
  } else {
    cat("⚠️ RMS package not available. Skipping reliability plot.\n")
  }
  
}, error = function(e) {
  cat("❌ Error creating RMS reliability plot:", e$message, "\n")
  # Close any open graphics devices in case of error
  if (dev.cur() != 1) dev.off()
})


# =============================================================================
# *** ACADEMIC VISUALIZATION & OUTPUT ***
# =============================================================================
# Create P-Values Histogram
cat("\n==============================================\n")
cat("P-VALUES HISTOGRAM\n")
cat("==============================================\n")

# Option to skip visualization if no new tests were run
if (length(valid_results) == 0) {
  cat("⚠️ No test results available for visualization.\n")
  cat("💡 Run some goodness-of-fit tests first to generate the histogram.\n")
} else {
  cat("📊 Creating visualization with", length(valid_results), "test results...\n")
  
  # Prepare data for histogram using stored results
  valid_results <- test_results[!sapply(test_results, is.na)]
  if (length(valid_results) > 0) {
    
    # Create data frame for plotting
    pvalues_df <- data.frame(
      Test = names(valid_results),
      P_Value = as.numeric(valid_results),
      stringsAsFactors = FALSE
    )
    
    # Sort by p-value (ascending)
    pvalues_df <- pvalues_df[order(pvalues_df$P_Value), ]
    
    # Create the plot
    library(ggplot2)
    library(scales)
    
    # Define academic color palette - dark green for significant, gradient to gray for non-significant
    pvalues_df$color_category <- ifelse(pvalues_df$P_Value < 0.05, "significant", "non_significant")
    
    # Apply custom test names using the mapping system
    pvalues_df$Test_Clean <- apply_custom_names(pvalues_df$Test)
    
    # Reorder factor levels for plotting with clean names
    pvalues_df$Test_Clean <- factor(pvalues_df$Test_Clean, levels = pvalues_df$Test_Clean)
    
    p_hist <- ggplot(pvalues_df, aes(y = Test_Clean, x = P_Value)) +
      geom_col(aes(fill = color_category), 
               width = 0.75, 
               color = "white", 
               size = 0.3,
               alpha = 0.9) +
      geom_vline(xintercept = 0.05, 
                 color = "#1b7837", 
                 linetype = "solid", 
                 size = 1.5,
                 alpha = 0.8) +
      geom_vline(xintercept = 0.10, 
                 color = "#fd8d3c", 
                 linetype = "dashed", 
                 size = 1.2,
                 alpha = 0.8) +
      scale_fill_manual(
        values = c("significant" = "#1b7837", "non_significant" = "#737373"),
        labels = c("significant" = expression(italic(p) < 0.05), "non_significant" = expression(italic(p) >= 0.05)),
        name = "Statistical Significance"
      ) +
      scale_x_continuous(
        limits = c(0, max(1, max(pvalues_df$P_Value) * 1.15)),
        breaks = c(0, 0.05, 0.10, 0.25, 0.50, 0.75, 1.0),
        labels = c("0.00", "0.05", "0.10", "0.25", "0.50", "0.75", "1.00"),
        expand = expansion(mult = c(0, 0.02))
      ) +
      labs(
        title = "Goodness-of-Fit Test Results: Distribution of P-Values",
        subtitle = expression(paste("Logistic regression model assessment (", italic(n), " = 188 observations)")),
        y = "Goodness-of-Fit Tests",
        x = expression(paste(italic("P"), "-value")),
        caption = expression(paste("Note: Dark green indicates ", italic(p), " < 0.05; Gray indicates ", italic(p), " ≥ 0.05"))
      ) +
      theme_minimal(base_size = 16) +  # Increased base font size
      theme(
        # Plot styling - increased all font sizes
        plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5, color = "gray30"),
        plot.caption = element_text(size = 14, color = "gray50", hjust = 0.5),
        
        # Axis styling - significantly increased font sizes
        axis.title.x = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 16, color = "black", face = "bold"),  # Increased X-axis numbers
        axis.text.y = element_text(size = 18, color = "black", hjust = 1, face = "bold"),  # Increased test names font size
        axis.line = element_line(color = "gray40", size = 0.7),
        axis.ticks = element_line(color = "gray40", size = 0.5),
        axis.ticks.length = unit(0.3, "cm"),
        
        # Legend styling - increased sizes
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm"),
        legend.background = element_rect(fill = "gray98", color = "gray80", size = 0.5),
        
        # Grid styling
        panel.grid.major.x = element_line(color = "gray90", size = 0.5, linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        
        # Panel border
        panel.border = element_blank()
      ) +
      # Add reference line annotations with increased font sizes
      annotate("text", 
               y = length(pvalues_df$Test_Clean) * 0.95, 
               x = 0.05 - 0.008, 
               label = expression(alpha == 0.05), 
               color = "#1b7837", 
               size = 6,  # Increased annotation size
               fontface = "bold",
               angle = 90, 
               vjust = 1.2,
               hjust = 1) +
      annotate("text", 
               y = length(pvalues_df$Test_Clean) * 0.95, 
               x = 0.10 - 0.008, 
               label = expression(alpha == 0.10), 
               color = "#fd8d3c", 
               size = 6,  # Increased annotation size
               fontface = "bold",
               angle = 90, 
               vjust = 1.2,
               hjust = 1) +
      # Add p-value labels with increased font size
      geom_text(aes(label = sprintf("%.3f", P_Value)),
                hjust = -0.05, 
                size = 6,    # Increased p-value label size from 4.5 to 6
                color = "black",
                fontface = "bold")
    
    # Save the plot with long/narrow dimensions (high length, low width)
    output_path <- "C:/Users/ebrah/.cursor-tutor/Proposal Tex/goodness_of_fit_pvalues_academic(with interaction).png"
    ggsave(
      filename = output_path,
      plot = p_hist,
      width = 18,  # Increased width from 8 to 10 inches
      height = 20, # Increased height for long figure
      dpi = 600,   # Higher DPI for publication
      bg = "white",
      device = "png"
    )
    
    # Also save as PDF for LaTeX documents with long/narrow dimensions
    output_path_pdf <- "C:/Users/ebrah/.cursor-tutor/Proposal Tex/goodness_of_fit_pvalues_academic(with interaction).pdf"
    ggsave(
      filename = output_path_pdf,
      plot = p_hist,
      width = 18,  # Increased width from 8 to 10 inches
      height = 20, # Increased height for long figure
      device = "pdf"
    )
    
    cat("Academic-quality histogram saved to:\n")
    cat("PNG:", output_path, "\n")
    cat("PDF:", output_path_pdf, "\n")
    
    # Display the plot
    print(p_hist)
    
    # Print summary statistics
    cat("\n--- P-Values Summary Statistics ---\n")
    cat("Total tests:", nrow(pvalues_df), "\n")
    cat("Tests with p < 0.05:", sum(pvalues_df$P_Value < 0.05), "\n")
    cat("Tests with p < 0.10:", sum(pvalues_df$P_Value < 0.10), "\n")
    cat("Minimum p-value:", sprintf("%.6f", min(pvalues_df$P_Value)), "\n")
    cat("Maximum p-value:", sprintf("%.6f", max(pvalues_df$P_Value)), "\n")
    cat("Median p-value:", sprintf("%.6f", median(pvalues_df$P_Value)), "\n")
    
  } else {
    cat("No valid p-values available for histogram.\n")
  }
}


# =============================================================================
# *** ANALYSIS COMPLETION & SUMMARY ***
# =============================================================================
cat("\n==============================================\n")
cat("🎉 ANALYSIS COMPLETE\n")
cat("==============================================\n")
cat("📊 CSV file:", csv_file_path, "\n")
cat("📈 Histogram:", "goodness_of_fit_pvalues_academic.png/pdf", "\n")
cat("💡 Tip: Next time you run this script, previously computed tests will be skipped!\n")

