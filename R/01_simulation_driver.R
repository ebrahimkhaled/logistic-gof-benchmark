library(stats)
library(parallel)
library(progress)
library(ResourceSelection)  # For hoslem.test
library(lmtest) 
library(statmod) #for score test

# Source files needed for testing - store paths for later use in parallel workers
source_file1 <- "c:\\Users\\ebrah\\.cursor-tutor\\projects\\LogisticDxWithoutSukel.r" #for MHL, HL-ftest, Osius-Rojek, Stukels-st, sst_both
source_file2 <- "c:\\Users\\ebrah\\.cursor-tutor\\projects\\lecessie1995.r" #for leCessie.test
source_file3 <- "c:\\Users\\ebrah\\.cursor-tutor\\projects\\pigeonheyse.r" #for pigeon_heyse_test   
source_file4 <- "c:\\Users\\ebrah\\.cursor-tutor\\projects\\Xie.R" #for XieGoodnessOfFitTest

# Source the files in the main process - with error handling
tryCatch({
  source(source_file1)
  source(source_file2)
  source(source_file3)
  source(source_file4)
  cat("Source files loaded successfully\n")
}, error = function(e) {
  cat("Error loading source files:", e$message, "\n")
})

# Load other required packages - with error handling
required_packages <- c("largesamplehl", "BAGofT", "rms", "ggplot2", "tidyr")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Package", pkg, "not available. Some functions may not work."))
  }
}

# Modified Hosmer-Lemeshow test with dynamic number of groups
modified_hosmer_lemeshow <- function(y, predicted_probs, min_expected = 5, max_iterations = 500) {
  n <- length(y)
  num_groups <- max(2, min(10, floor(0.05 * n)))  # Dynamic number of groups
  
  sorted_indices <- order(predicted_probs)
  sorted_y <- y[sorted_indices]
  sorted_probs <- predicted_probs[sorted_indices]
  
  group_size <- floor(n / num_groups)
  
  groups <- rep(1:num_groups, each = group_size)
  if (length(groups) < n) {
    groups <- c(groups, rep(num_groups, n - length(groups)))
  }
  
  group_expected <- tapply(sorted_probs, groups, sum)
  
  # Ensure all groups have sum of expected > min_expected
  iteration <- 0
  while (any(group_expected < min_expected) && iteration < max_iterations) {
    small_group <- which.min(group_expected)
    large_group <- which.max(group_expected)
    
    move_idx <- which(groups == large_group)[1]
    groups[move_idx] <- small_group
    
    group_expected[small_group] <- group_expected[small_group] + sorted_probs[move_idx]
    group_expected[large_group] <- group_expected[large_group] - sorted_probs[move_idx]
    
    iteration <- iteration + 1
  }
  
  if (iteration == max_iterations) {
    warning("Maximum iterations reached. Groups may not satisfy minimum expected condition.")
  }
  
  observed <- tapply(sorted_y, groups, sum)
  expected <- tapply(sorted_probs, groups, sum)
  n_group <- tapply(sorted_y, groups, length)
  
  chi_squared <- sum((observed - expected)^2 / (expected * (1 - expected / n_group)))
  df <- num_groups - 2
  p_value <- 1 - pchisq(chi_squared, df)
  
  return(list(chi_squared = chi_squared, df = df, p_value = p_value, groups = groups, observed = observed, expected = expected, n = n_group))
}

# Function to perform score test - FIXED
score_test <- function(groups, y, predicted_probs, model) {
  # Convert groups to dummy variables
  group_dummies <- model.matrix(~ factor(groups) - 1)
  
  # Fit the model using glm.scoretest with error handling
  tryCatch({
    score_test_result <- glm.scoretest(model, group_dummies)
    df = length(score_test_result)
    X2 = sum(score_test_result^2)  # FIXED: using sum() instead of matrix multiplication
    p_value = 1 - pchisq(X2, df)
    return(p_value)
  }, error = function(e) {
    warning(paste("Score test failed:", e$message))
    return(NA)
  })
}

# Function to simulate data and perform tests - FIXED
simulate_and_test <- function(n, seed = NULL) {
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Set parameters
  beta0 <- 0
  beta1 <- -0.7
  beta2 <- 0.7
  beta3 <- 0.5
  beta4 <- -0.5
  beta5 <- 0.3
  beta6 <- -0.3
  
  x1 <- runif(n, -4, 4)
  x2 <- rnorm(n, 0, 4)
  x3 <- runif(n, 1, 3)
  x4 <- rnorm(n, 2, 1)
  x5 <- rbinom(n, 1, 0.5)
  x6 <- rexp(n, 1)
  
  z <- beta0 + beta1*x1 + beta2*x2 + beta3*x3 + beta4*x4 + beta5*x5 + beta6*x6
  prob <- 1 / (1 + exp(-z))
  y <- rbinom(n, 1, prob)

  formula <- as.formula(y ~ x1 + x2 + x3 + x4 + x5 + x6)
  
  model <- glm(formula, family = binomial(link = "logit"))
  predicted_probs <- predict(model, type = "response")
  
  # Initialize results with NA
  results <- rep(NA, 18)
  names(results) <- c(
    "traditional_HL", "modified_HL", "pearson", "deviance", "score_test",
    "hl_ftest", "osius_rojek", "s_st_pgeq0_5", "s_st_pl0_5", "sst_both",
    "large_s_hl", "lecessie", "bagofT", "spiegelhalter_10_g",
    "spiegelhalter", "Copas_unweighted_S", "pigeonheyse", "xie"
  )
  
  # Run tests with error handling for each test
  
  # 1. Traditional Hosmer-Lemeshow test
  tryCatch({
    traditional_hl <- hoslem.test(y, predicted_probs)
    results["traditional_HL"] <- traditional_hl$p.value
  }, error = function(e) {
    warning(paste("Traditional Hosmer-Lemeshow test failed:", e$message))
  })
  
  # 2. Modified Hosmer-Lemeshow test
  tryCatch({
    modified_hl <- modified_hosmer_lemeshow(y, predicted_probs)
    results["modified_HL"] <- modified_hl$p_value
    
    # 5. Score test - only run if modified_hl worked
    tryCatch({
      results["score_test"] <- score_test(modified_hl$groups, y, predicted_probs, model)
    }, error = function(e) {
      warning(paste("Score test failed:", e$message))
    })
  }, error = function(e) {
    warning(paste("Modified Hosmer-Lemeshow test failed:", e$message))
  })
  
  # 3. Pearson chi-square test
  tryCatch({
    pearson_residuals <- residuals(model, type = "pearson")
    pearson_chi_square <- sum(pearson_residuals^2)
    results["pearson"] <- 1 - pchisq(pearson_chi_square, df = model$df.residual)
  }, error = function(e) {
    warning(paste("Pearson chi-square test failed:", e$message))
  })
  
  # 4. Deviance test
  tryCatch({
    results["deviance"] <- 1 - pchisq(model$deviance, df = model$df.residual)
  }, error = function(e) {
    warning(paste("Deviance test failed:", e$message))
  })
  
  # 6-10. Tests from gof.glm
  tryCatch({
    if (exists("gof.glm")) {
      gof_results <- suppressWarnings(gof.glm(model, plotROC = FALSE))
      if (!is.null(gof_results) && !is.null(gof_results$gof) && !is.null(gof_results$gof$pVal)) {
        results["hl_ftest"] <- gof_results$gof$pVal[2]
        results["osius_rojek"] <- gof_results$gof$pVal[3]
        results["s_st_pgeq0_5"] <- gof_results$gof$pVal[4]
        results["s_st_pl0_5"] <- gof_results$gof$pVal[5]
        results["sst_both"] <- gof_results$gof$pVal[6]
      }
    }
  }, error = function(e) {
    warning(paste("gof.glm tests failed:", e$message))
  })
  
  # 11. Large sample Hosmer-Lemeshow test
  tryCatch({
    if (exists("hltest")) {
      hl_result <- hltest(model)
      if (!is.null(hl_result) && !is.null(hl_result$p.value)) {
        results["large_s_hl"] <- hl_result$p.value
      }
    }
  }, error = function(e) {
    warning(paste("Large sample Hosmer-Lemeshow test failed:", e$message))
  })
  
  # 12. Le Cessie test
  tryCatch({
    if (exists("leCessie.test")) {
      lecessie_result <- leCessie.test(model)
      if (!is.null(lecessie_result) && !is.null(lecessie_result$p.value) && length(lecessie_result$p.value) > 0) {
        results["lecessie"] <- lecessie_result$p.value[[1]]
      }
    }
  }, error = function(e) {
    warning(paste("Le Cessie test failed:", e$message))
  })

  # 13. BAGofT test
  tryCatch({
    if (exists("BAGofT") && exists("testGlmBi") && exists("parRF")) {
      # Extract the terms (variables) from the formula
      variables <- all.vars(formula)
      
      # Initialize an empty list to store columns
      data_list <- list()
      
      # Loop through the variables and assign the corresponding data to the list
      for (var in variables) {
        data_list[[var]] <- get(var)
      }
      
      # Convert the list to a dataframe
      df <- as.data.frame(data_list)
      
      bagofT_result <- suppressWarnings(BAGofT(testModel = testGlmBi(formula = formula, link = "logit"),
                         parFun = parRF(), data = df, nsim = 1))
      if (!is.null(bagofT_result) && !is.null(bagofT_result$p.value)) {
        results["bagofT"] <- bagofT_result$p.value
      }
    }
  }, error = function(e) {
    warning(paste("BAGofT test failed:", e$message))
  })
  
  # 14-15. Spiegelhalter's z-test
  tryCatch({
    if (exists("val.prob")) {
      # With groups
      spg_result_with_groups <- val.prob(predicted_probs, y, g=10, pl = FALSE)
      if (length(spg_result_with_groups) >= 18 && !is.null(spg_result_with_groups[[18]])) {
        results["spiegelhalter_10_g"] <- spg_result_with_groups[[18]][1]
      }
      
      # Without groups
      spg_result_without_groups <- val.prob(predicted_probs, y, pl = FALSE)
      if (length(spg_result_without_groups) >= 18 && !is.null(spg_result_without_groups[[18]])) {
        results["spiegelhalter"] <- spg_result_without_groups[[18]][1]
      }
    }
  }, error = function(e) {
    warning(paste("Spiegelhalter's z-test failed:", e$message))
  })
  
  # 16. Copas unweighted S test
  tryCatch({
    if (exists("lrm") && exists("resid")) {
      copas_result <- resid(lrm(formula, x=TRUE, y=TRUE), "gof")
      if (length(copas_result) >= 5) {
        results["Copas_unweighted_S"] <- copas_result[5]
      }
    }
  }, error = function(e) {
    warning(paste("Copas unweighted S test failed:", e$message))
  })
  
  # 17. Pigeon-Heyse test
  tryCatch({
    if (exists("pigeon_heyse_test")) {
      # We already created df above for BAGofT
      if (!exists("df")) {
        variables <- all.vars(formula)
        data_list <- list()
        for (var in variables) {
          data_list[[var]] <- get(var)
        }
        df <- as.data.frame(data_list)
      }
      
      ph_result <- pigeon_heyse_test(df, model)
      if (!is.null(ph_result) && !is.null(ph_result$p_value)) {
        results["pigeonheyse"] <- ph_result$p_value
      }
    }
  }, error = function(e) {
    warning(paste("Pigeon-Heyse test failed:", e$message))
  })
  
  # 18. Xie test
  tryCatch({
    if (exists("XieGoodnessOfFitTest")) {
      xie_data <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = x5, x6 = x6)
      xie_result <- XieGoodnessOfFitTest(xie_data, list(predicted_probs = predicted_probs))
      if (!is.null(xie_result) && !is.null(xie_result)) {
        results["xie"] <- xie_result
      }
    }
  }, error = function(e) {
    warning(paste("Xie test failed:", e$message))
  })

  return(results)
}

# Function to run a single simulation replication
run_one_sim <- function(seed, n) {
  set.seed(seed)
  result <- tryCatch({
    simulate_and_test(n)
  }, error = function(e) {
    message(paste("Error in simulation with seed", seed, ":", e$message))
    return(rep(NA, 18))  # Return NAs for all tests
  })
  return(result)
}

# NEW FUNCTIONS FOR INCREMENTAL ANALYSIS

# Function to create file name for a given sample size
get_filename <- function(n, output_dir) {
  return(file.path(output_dir, paste0("pvalues_n", n, ".csv")))
}

# Function to load existing results or create new data frame
load_or_create_results <- function(filename, test_names) {
  if (file.exists(filename)) {
    existing_data <- read.csv(filename, stringsAsFactors = FALSE)
    cat("Loaded existing file with", nrow(existing_data), "iterations\n")
    return(existing_data)
  } else {
    # Create new data frame with column names
    new_data <- data.frame(matrix(ncol = length(test_names), nrow = 0))
    colnames(new_data) <- test_names
    cat("Created new file\n")
    return(new_data)
  }
}

# Function to append new results to existing data
append_results <- function(existing_data, new_results_matrix, filename) {
  # Convert matrix to data frame
  new_df <- as.data.frame(t(new_results_matrix))
  
  # Ensure column names match
  if (ncol(existing_data) == ncol(new_df)) {
    colnames(new_df) <- colnames(existing_data)
  }
  
  # Combine old and new data
  combined_data <- rbind(existing_data, new_df)
  
  # Save to file
  write.csv(combined_data, filename, row.names = FALSE)
  
  cat("Appended", nrow(new_df), "new iterations. Total iterations now:", nrow(combined_data), "\n")
  return(combined_data)
}

# Function to calculate and display power analysis from saved files
analyze_saved_results <- function(sample_sizes, output_dir, alpha = 0.05) {
  cat("\n" %+% paste(rep("=", 60), collapse = "") %+% "\n")
  cat("POWER ANALYSIS FROM SAVED FILES\n")
  cat(paste(rep("=", 60), collapse = "") %+% "\n")
  
  results_summary <- data.frame()
  
  for (n in sample_sizes) {
    filename <- get_filename(n, output_dir)
    
    if (file.exists(filename)) {
      data <- read.csv(filename, stringsAsFactors = FALSE)
      n_iterations <- nrow(data)
      
      cat("\nSample size n =", n, "(", n_iterations, "iterations )\n")
      cat(paste(rep("-", 40), collapse = "") %+% "\n")
      
      # Calculate power for each test
      power_values <- sapply(data, function(x) {
        valid_pvals <- x[!is.na(x)]
        if (length(valid_pvals) == 0) return(0)
        return(mean(valid_pvals < alpha))
      })
      
      # Calculate availability (proportion of non-NA values)
      availability <- sapply(data, function(x) mean(!is.na(x)))
      
      # Create summary for this sample size
      test_summary <- data.frame(
        Test = names(power_values),
        Power = round(power_values, 4),
        Availability = round(availability, 3),
        Valid_Tests = sapply(data, function(x) sum(!is.na(x))),
        stringsAsFactors = FALSE
      )
      
      # Add sample size info
      test_summary$n <- n
      test_summary$Total_Iterations <- n_iterations
      
      # Display results for this sample size
      print(test_summary[order(-test_summary$Power), c("Test", "Power", "Availability", "Valid_Tests")])
      
      # Add to overall summary
      results_summary <- rbind(results_summary, test_summary)
      
    } else {
      cat("\nSample size n =", n, ": No saved results found\n")
    }
  }
  
  if (nrow(results_summary) > 0) {
    cat("\n" %+% paste(rep("=", 60), collapse = "") %+% "\n")
    cat("OVERALL SUMMARY\n")
    cat(paste(rep("=", 60), collapse = "") %+% "\n")
    
    # Create wide format summary table
    power_wide <- reshape(results_summary[, c("Test", "n", "Power")], 
                         idvar = "Test", timevar = "n", direction = "wide")
    colnames(power_wide) <- c("Test", paste0("n_", sample_sizes[sample_sizes %in% unique(results_summary$n)]))
    
    print(power_wide)
    
    # Save overall summary
    summary_file <- file.path(output_dir, "power_analysis_summary.csv")
    write.csv(results_summary, summary_file, row.names = FALSE)
    cat("\nDetailed summary saved to:", summary_file, "\n")
    
    return(results_summary)
  }
  
  return(NULL)
}

# Set parameters for simulation
sample_sizes <- c(200, 500, 1000, 3000)
num_replications <- 980  # Number of NEW replications to add this run
alpha <- 0.05

# Get the directory of the source files to save results there
output_dir <- dirname(source_file1)
cat("Output directory:", output_dir, "\n")

# Test names for consistency
test_names <- c(
  "traditional_HL", "modified_HL", "pearson", "deviance", "score_test",
  "hl_ftest", "osius_rojek", "s_st_pgeq0_5", "s_st_pl0_5", "sst_both",
  "large_s_hl", "lecessie", "bagofT", "spiegelhalter_10_g",
  "spiegelhalter", "Copas_unweighted_S", "pigeonheyse", "xie"
)

# Set up parallel cluster
cat("\nSetting up parallel processing...\n")
num_cores <- detectCores() - 1
cat("Using", num_cores, "CPU cores for parallel processing\n")
cl <- makeCluster(num_cores)

# Load all required packages on each worker
cat("Loading packages on workers...\n")
clusterEvalQ(cl, {
  # Load all necessary libraries on each worker
  library(stats)
  library(ResourceSelection)
  library(lmtest)
  library(statmod)
  
  # Try to load additional packages
  required_packages <- c("largesamplehl", "BAGofT", "rms", "ggplot2", "tidyr")
  for (pkg in required_packages) {
    suppressMessages(suppressWarnings(
      require(pkg, character.only = TRUE, quietly = TRUE)
    ))
  }
  
  # Return TRUE to confirm
  TRUE
})

# Source files on each worker
cat("Sourcing files on workers...\n")
clusterExport(cl, c("source_file1", "source_file2", "source_file3", "source_file4"))

clusterEvalQ(cl, {
  # Source all the files on each worker
  tryCatch({
    source(source_file1)
    source(source_file2)
    source(source_file3)
    source(source_file4)
    TRUE
  }, error = function(e) {
    message(paste("Error loading source files on worker:", e$message))
    FALSE
  })
})

# Export all necessary functions to workers
clusterExport(cl, c(
  "simulate_and_test", "modified_hosmer_lemeshow", "score_test",
  "run_one_sim", "alpha"
))

# Test worker functionality
cat("Testing worker functionality...\n")
test_result <- tryCatch({
  worker_test <- parLapply(cl, 1:min(3, num_cores), function(i) {
    tryCatch({
      set.seed(1234 + i)
      result <- simulate_and_test(50)
      non_na_count <- sum(!is.na(result))
      if (non_na_count >= 5) {
        return(list(success = TRUE, non_na_count = non_na_count))
      } else {
        return(list(success = FALSE, non_na_count = non_na_count, result = result))
      }
    }, error = function(e) {
      return(list(success = FALSE, error = e$message))
    })
  })
  
  all_ok <- all(sapply(worker_test, function(x) x$success))
  if (!all_ok) {
    cat("Worker test results:\n")
    for (i in seq_along(worker_test)) {
      cat("Worker", i, ":", 
          if (worker_test[[i]]$success) "SUCCESS" else "FAILED", 
          "(", worker_test[[i]]$non_na_count, "tests worked)\n")
    }
    cat("Some workers may have limited functionality, but proceeding...\n")
  } else {
    cat("All workers passed comprehensive simulation test\n")
  }
  TRUE
}, error = function(e) {
  cat("Worker test failed with error:", e$message, "\n")
  FALSE
})

if (!test_result) {
  stopCluster(cl)
  stop("Worker testing failed completely. Please check for errors above.")
}

cat("Proceeding with simulations...\n")

# MAIN SIMULATION LOOP - Modified for incremental saving
for (i in seq_along(sample_sizes)) {
  n <- sample_sizes[i]
  filename <- get_filename(n, output_dir)
  
  cat("\n" %+% paste(rep("=", 50), collapse = "") %+% "\n")
  cat("Processing sample size n =", n, "\n")
  cat(paste(rep("=", 50), collapse = "") %+% "\n")
  
  # Load existing results or create new dataframe
  existing_results <- load_or_create_results(filename, test_names)
  
  # Generate seeds for new simulations
  set.seed(1234 + i + nrow(existing_results))  # Use existing count to ensure different seeds
  seeds <- sample.int(1e6, num_replications)
  
  # Create arguments list for parLapply
  args_list <- lapply(seeds, function(s) list(seed = s, n = n))
  
  # Run new simulations in parallel
  cat("Running", num_replications, "NEW replications in parallel...\n")
  
  results_list <- parLapply(cl, args_list, function(args) {
    tryCatch({
      run_one_sim(args$seed, args$n)
    }, error = function(e) {
      message(paste("Error in parallel simulation:", e$message))
      return(rep(NA, 18))
    })
  })
  
  # Convert list of results to matrix
  results_matrix <- do.call(cbind, results_list)
  
  # Append new results to existing data and save
  combined_results <- append_results(existing_results, results_matrix, filename)
  
  # Display diagnostics for this run
  successful_sims <- sum(apply(results_matrix, 2, function(x) !all(is.na(x))))
  cat("Completed", successful_sims, "out of", num_replications, "new simulations successfully\n")
  
  # Show summary of which tests worked in this batch
  test_success_rate <- rowMeans(!is.na(results_matrix))
  cat("Test success rates for this batch:\n")
  for (j in seq_along(test_success_rate)) {
    cat(sprintf("  %s: %.1f%%\n", names(test_success_rate)[j], test_success_rate[j] * 100))
  }
}

# Stop cluster
stopCluster(cl)

# Perform comprehensive analysis of all saved results
cat("\n" %+% paste(rep("*", 70), collapse = "") %+% "\n")
cat("PERFORMING COMPREHENSIVE ANALYSIS OF ALL SAVED RESULTS\n")
cat(paste(rep("*", 70), collapse = "") %+% "\n")

final_summary <- analyze_saved_results(sample_sizes, output_dir, alpha)

cat("\nSimulation completed successfully!\n")
cat("Individual p-values saved for each sample size in separate CSV files:\n")
for (n in sample_sizes) {
  filename <- get_filename(n, output_dir)
  if (file.exists(filename)) {
    data <- read.csv(filename)
    cat(sprintf("- %s (%d iterations)\n", basename(filename), nrow(data)))
  }
}

cat("\nTo run more iterations later, simply run this script again!\n")
cat("Results will be automatically appended to existing files.\n")

# Optional: Create visualization if we have results
if (!is.null(final_summary) && nrow(final_summary) > 0) {
  cat("\nCreating visualization...\n")
  
  # Load required libraries for plotting
  library(ggplot2)
  library(tidyr)
  library(dplyr)
  library(viridis)
  
  # Prepare data for heatmap
  heatmap_data <- final_summary %>%
    select(Test, n, Power) %>%
    mutate(
      test_clean = case_when(
        Test == "traditional_HL" ~ "Traditional HL",
        Test == "modified_HL" ~ "Modified HL", 
        Test == "pearson" ~ "Pearson χ²",
        Test == "deviance" ~ "Deviance",
        Test == "score_test" ~ "Score Test",
        Test == "hl_ftest" ~ "HL F-test",
        Test == "osius_rojek" ~ "Osius-Rojek",
        Test == "s_st_pgeq0_5" ~ "Stukel ≥0.5",
        Test == "s_st_pl0_5" ~ "Stukel <0.5", 
        Test == "sst_both" ~ "Stukel Both",
        Test == "large_s_hl" ~ "Large Sample HL",
        Test == "lecessie" ~ "Le Cessie",
        Test == "bagofT" ~ "BAGofT",
        Test == "spiegelhalter_10_g" ~ "Spiegelhalter (10g)",
        Test == "spiegelhalter" ~ "Spiegelhalter",
        Test == "Copas_unweighted_S" ~ "Copas Unweighted",
        Test == "pigeonheyse" ~ "Pigeon-Heyse",
        Test == "xie" ~ "Xie Test",
        TRUE ~ Test
      ),
      n_factor = factor(n, levels = sort(unique(n))),
      # Create categories for power levels
      power_category = case_when(
        Power == 0 ~ "No Power (0%)",
        Power > 0 & Power <= 0.05 ~ "Very Low (0-5%)",
        Power > 0.05 & Power <= 0.20 ~ "Low (5-20%)",
        Power > 0.20 & Power <= 0.50 ~ "Moderate (20-50%)",
        Power > 0.50 & Power <= 0.80 ~ "Good (50-80%)",
        Power > 0.80 ~ "Excellent (>80%)",
        TRUE ~ "Unknown"
      )
    )
  
  # Create the main heatmap with continuous scale
  p1 <- ggplot(heatmap_data, aes(x = n_factor, y = reorder(test_clean, Power), fill = Power)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_viridis_c(
      name = "Power",
      labels = scales::percent_format(accuracy = 1),
      option = "plasma",
      trans = "sqrt",  # Square root transformation to better show low values
      na.value = "grey90"
    ) +
    labs(
      title = "Goodness-of-Fit Tests: Power Analysis Heatmap",
      subtitle = "Power to detect model misspecification across sample sizes",
      x = "Sample Size (n)",
      y = "Statistical Test",
      caption = "Higher values indicate better ability to detect poor model fit"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 11, face = "bold"),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      panel.grid = element_blank(),
      plot.caption = element_text(size = 8, color = "grey60")
    ) +
    # Add text annotations showing exact power values
    geom_text(aes(label = ifelse(Power > 0, paste0(round(Power*100, 1), "%"), "0%")), 
              color = "white", size = 3, fontface = "bold")
  
  # Create a categorical version for easier interpretation
  p2 <- ggplot(heatmap_data, aes(x = n_factor, y = reorder(test_clean, Power), fill = power_category)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_manual(
      name = "Power Level",
      values = c(
        "No Power (0%)" = "#440154",
        "Very Low (0-5%)" = "#3b528b", 
        "Low (5-20%)" = "#21908c",
        "Moderate (20-50%)" = "#5dc863",
        "Good (50-80%)" = "#fde725",
        "Excellent (>80%)" = "#f0f921"
      ),
      na.value = "grey90"
    ) +
    labs(
      title = "Goodness-of-Fit Tests: Power Categories",
      subtitle = "Categorical view of test performance",
      x = "Sample Size (n)",
      y = "Statistical Test"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 11, face = "bold"),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8),
      panel.grid = element_blank()
    )
  
  # Create a power trend plot
  p3 <- ggplot(heatmap_data, aes(x = n, y = Power, color = test_clean)) +
    geom_line(size = 1, alpha = 0.7) +
    geom_point(size = 2) +
    scale_color_viridis_d(name = "Test", option = "turbo") +
    scale_x_continuous(breaks = unique(heatmap_data$n)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Power Trends Across Sample Sizes",
      subtitle = "How test power changes with increasing sample size",
      x = "Sample Size (n)",
      y = "Power (%)",
      caption = "Lines show power trajectory for each test"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      legend.position = "right",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 8)
    ) +
    guides(color = guide_legend(ncol = 1))
  
  # Display the plots
  print(p1)
  print(p2)
  print(p3)
  
  # Save the plots
  tryCatch({
    ggsave(file.path(output_dir, "power_analysis_heatmap_continuous.png"), 
           plot = p1, width = 10, height = 8, dpi = 300)
    ggsave(file.path(output_dir, "power_analysis_heatmap_categorical.png"), 
           plot = p2, width = 10, height = 8, dpi = 300)
    ggsave(file.path(output_dir, "power_trends_by_sample_size.png"), 
           plot = p3, width = 12, height = 8, dpi = 300)
    
    cat("\nVisualization plots created and saved to:", output_dir, "\n")
    cat("Files created:\n")
    cat("- power_analysis_heatmap_continuous.png\n")
    cat("- power_analysis_heatmap_categorical.png\n") 
    cat("- power_trends_by_sample_size.png\n")
  }, error = function(e) {
    cat("Error saving plots:", e$message, "\n")
    cat("Plots displayed but not saved.\n")
  })
  
  # Create summary statistics table
  tryCatch({
    summary_stats <- heatmap_data %>%
      group_by(test_clean) %>%
      summarise(
        mean_power = mean(Power, na.rm = TRUE),
        max_power = max(Power, na.rm = TRUE),
        min_power = min(Power, na.rm = TRUE),
        power_range = max_power - min_power,
        .groups = 'drop'
      ) %>%
      arrange(desc(mean_power))
    
    cat("\nSummary Statistics by Test:\n")
    print(summary_stats)
    
    # Save summary statistics
    write.csv(summary_stats, file.path(output_dir, "test_summary_statistics.csv"), row.names = FALSE)
    cat("\nSummary statistics saved to: test_summary_statistics.csv\n")
    
  }, error = function(e) {
    cat("Error creating summary statistics:", e$message, "\n")
  })
  
} else {
  cat("No results available for visualization.\n")
}