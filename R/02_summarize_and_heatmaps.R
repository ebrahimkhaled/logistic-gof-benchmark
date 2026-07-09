# =====================
# SUMMARY FOR ALL SCENARIOS AND ALL TESTS (Type 1, OmitVar, OmitInteract)
# =====================

# =============================================================================
# CONFIGURATION: TESTS TO EXCLUDE FROM HEAT MAPS
# =============================================================================
# Define tests that should be excluded from all heat map visualizations
# HOSMER METHODOLOGY COMPARISON: Keep only selected tests
tests_to_exclude_from_heatmaps <- c(
  # EXCLUDE all tests EXCEPT the 7 selected for Hosmer methodology comparison:
  # KEEP: Final_Farrington_G10, xie, bagoft_parallel, Tsiatis_clustering, 
  #       traditional_HL, hosmer_equal_width, pigeonheyse
  
  # All other tests to EXCLUDE:
  "giviti_calib",
  "giviti_calib_internal", 
  "pearson",
  "deviance",
  "hl_ftest",
  "osius_rojek",
  "s_st_pgeq0_5",
  "s_st_pl0_5",
  "sst_both",
  "large_s_hl",
  "spiegelhalter",
  "Copas_unweighted_S",
  "eHL",
  "eHL_10",
  "U_p",
  "HL_GAM",
  "PR_GAM", 
  "XIE_GAM",
  "Farrington_standalone",
  "IM_efficient",
  "McCullagh_Test",
  "leCessie_Test",
  "bagoft_test_NO_SIM_Split_1",
  "bagoft_test_Split_20",
  "bagoft_parallel",
  "hosmer_equal_width",
  "xie",
  "stute_zhu_test",
  "PR_test",
  "hosmer_bootstrap",
  "Tsiatis_clustering",
  
  # Additional Farrington variants (keep only G=10)
  "Final_Farrington_G4",
  "Final_Farrington_G40",
  "Final_Farrington_standalone"
  
  # FINAL INCLUDED TESTS (7 total):
  # ✅ Final_Farrington_G10 - Ebrahim-Farrington Test (G=10)
  # ✅ xie - Xie Test  
  # ✅ bagoft_parallel - BAGofT Test (20 splits, 100 simulations)
  # ✅ Tsiatis_clustering - Tsiatis Clustering Test
  # ✅ traditional_HL - Traditional Hosmer-Lemeshow (equal size groups)
  # ✅ hosmer_equal_width - Hosmer-Lemeshow Equal Width (equal intervals)
  # ✅ pigeonheyse - Pigeon-Heyse Test
)

# Display current exclusion list for confirmation
cat("=== HEAT MAP EXCLUSION CONFIGURATION ===\n")
cat("The following tests will be EXCLUDED from all heat maps:\n")
if (length(tests_to_exclude_from_heatmaps) > 0) {
  for (i in seq_along(tests_to_exclude_from_heatmaps)) {
    cat(sprintf("  %d. %s\n", i, tests_to_exclude_from_heatmaps[i]))
  }
} else {
  cat("  (No tests excluded - all tests will appear in heat maps)\n")
}
cat("==========================================\n\n")

# =============================================================================
# SUMMARY GENERATION FUNCTIONS
# =============================================================================

summarize_scenarios_all_tests <- function(output_dir) {
  files <- list.files(output_dir, pattern = "^pvalues_.*_n[0-9]+\\.csv$", full.names = TRUE)
  extract_info <- function(f) {
    m <- regexec("pvalues_(.*)_n([0-9]+)\\.csv$", basename(f))
    parts <- regmatches(basename(f), m)[[1]]
    if (length(parts) == 3) {
      data.frame(file = f, scenario = parts[2], n = as.integer(parts[3]), stringsAsFactors = FALSE)
    } else {
      NULL
    }
  }
  info <- do.call(rbind, lapply(files, extract_info))
  if (is.null(info) || nrow(info) == 0) {
    cat("No matching files found in", output_dir, "\n")
    return(NULL)
  }
  # Define scenario groups
  type1_scenarios <- c("Uniform_-6_6", "Uniform_-3_3", "Normal_0_1.5", "Chi2_4", "Multi_Indep")
  omitvar_scenarios <- c("Quad_Slight", "Quad_Pronounced")
  omitinteract_scenarios <- c("Interact_Slight", "Interact_Pronounced")
  summarize_group <- function(scenarios, info) {
    summary_list <- list()
    for (sc in scenarios) {
      sc_info <- info[info$scenario == sc, ]
      if (is.null(sc_info) || nrow(sc_info) == 0) next
      for (i in seq_len(nrow(sc_info))) {
        f <- sc_info$file[i]
        n <- sc_info$n[i]
        if (file.exists(f)) {
          data <- read.csv(f)
          for (test in names(data)) {
            pvals <- as.numeric(as.character(unlist(data[[test]])))
            pvals <- pvals[!is.na(pvals)]
            n_rep <- length(pvals)
            power <- mean(pvals < 0.05, na.rm = TRUE)
            summary_list[[paste(sc, test, n, sep = "_")]] <- data.frame(
              Scenario = sc,
              Test = test,
              n = n,
              Replications = n_rep,
              Power = round(power, 4),
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    if (length(summary_list) > 0) do.call(rbind, summary_list) else NULL
  }
  # Summarize and export
  type1_summary <- summarize_group(type1_scenarios, info)
  omitvar_summary <- summarize_group(omitvar_scenarios, info)
  omitinteract_summary <- summarize_group(omitinteract_scenarios, info)
  if (!is.null(type1_summary))
    write.csv(type1_summary, file.path(output_dir, "type1error_summary_alln.csv"), row.names = FALSE)
  if (!is.null(omitvar_summary))
    write.csv(omitvar_summary, file.path(output_dir, "power_summary_omitted_variable_alln.csv"), row.names = FALSE)
  if (!is.null(omitinteract_summary))
    write.csv(omitinteract_summary, file.path(output_dir, "power_summary_omitted_interaction_alln.csv"), row.names = FALSE)
  cat("Summary CSVs written:\n")
  if (!is.null(type1_summary)) cat("-", file.path(output_dir, "type1error_summary_alln.csv"), "\n")
  if (!is.null(omitvar_summary)) cat("-", file.path(output_dir, "power_summary_omitted_variable_alln.csv"), "\n")
  if (!is.null(omitinteract_summary)) cat("-", file.path(output_dir, "power_summary_omitted_interaction_alln.csv"), "\n")
}

# Call this at the end:
summarize_scenarios_all_tests("C:/Users/ebrah/.cursor-tutor/projects/")

# =====================
# VISUALIZATION FOR EACH SUMMARY CSV (SEPARATE PLOTS FOR TYPE 1 ERROR SCENARIOS)
# =====================
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(scales)

# =============================================================================
# CUSTOM COLOR SCALE FOR TYPE I ERROR HEAT MAPS
# =============================================================================

# Create custom color function for Type I Error visualization
create_type1_color_scale <- function() {
  # Define color breakpoints and colors according to your specification
  # 0% = Dark Blue (#5111FC)
  # 5% = Green (#287D47) 
  # 15% = Red (#FC5951)
  # >15% = Same red (#FC5951)
  
  colors <- c("#5111FC", "#287D47", "#FC5951", "#FC5951")
  breaks <- c(0, 0.05, 0.105, 1.0)
  
  scale_fill_gradientn(
    name = "Type I Error\nRate",
    colors = colors,
    values = breaks,
    labels = scales::percent_format(accuracy = 1),
    na.value = "grey90",
    limits = c(0, max(0.25, 1)), # Ensure we can see variations up to 25% or max value
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 15,
      barheight = 1.5
    )
  )
}

# =============================================================================
# TYPE I ERROR SPECIFIC PLOTTING FUNCTION
# =============================================================================

plot_type1_error_heatmap <- function(summary_csv, output_png, scenarios_filter = NULL, plot_title = "Type I Error Analysis") {
  df <- read.csv(summary_csv, stringsAsFactors = FALSE)
  
  # Filter scenarios if specified
  if (!is.null(scenarios_filter)) {
    df <- df[df$Scenario %in% scenarios_filter, ]
  }
  
  # =============================================================================
  # APPLY TEST EXCLUSIONS FOR HEAT MAPS
  # =============================================================================
  # Remove tests that are in the exclusion list
  if (exists("tests_to_exclude_from_heatmaps") && length(tests_to_exclude_from_heatmaps) > 0) {
    original_test_count <- length(unique(df$Test))
    df <- df[!df$Test %in% tests_to_exclude_from_heatmaps, ]
    excluded_count <- original_test_count - length(unique(df$Test))
    
    if (excluded_count > 0) {
      cat(sprintf("Heat map generation: Excluded %d test(s) from visualization\n", excluded_count))
    }
  }
  
  # Check if we have any data left after exclusions
  if (nrow(df) == 0) {
    cat("Warning: No data remaining after applying exclusions. Skipping plot generation.\n")
    return(invisible(NULL))
  }
  
  # Clean up test names for display - HOSMER METHODOLOGY COMPARISON
  df$Test_clean <- dplyr::recode(df$Test,
    # PRIMARY TESTS FOR HOSMER METHODOLOGY COMPARISON
    Final_Farrington_G10 = "Ebrahim-Farrington (G=10)",
    xie = "Xie Test",
    bagoft_parallel = "BAGofT (Split=20, B=100)",
    Tsiatis_clustering = "Tsiatis Score Test", 
    traditional_HL = "Hosmer-Lemeshow (Equal Size)",
    hosmer_equal_width = "Hosmer-Lemeshow (Equal Width)",
    pigeonheyse = "Pigeon-Heyse Test",
    
    # OTHER TESTS (should be excluded but include mapping for completeness)
    giviti_calib = "GIVITI Calibration (external)",
    giviti_calib_internal = "GIVITI Calibration (internal)",
    pearson = "Pearson χ²",
    deviance = "Deviance",
    hl_ftest = "HL F-test",
    osius_rojek = "Osius-Rojek",
    s_st_pgeq0_5 = "Stukel ≥0.5",
    s_st_pl0_5 = "Stukel <0.5", 
    sst_both = "Stukel Both",
    large_s_hl = "Large Sample HL",
    spiegelhalter = "Spiegelhalter",
    Copas_unweighted_S = "Copas Unweighted",
    eHL = "eHL (20)",
    eHL_10 = "eHL (10)",
    U_p = "Unreliability Index",
    HL_GAM = "HL+GAM",
    PR_GAM = "PR+GAM",
    XIE_GAM = "XIE+GAM",
    Farrington_standalone = "Farrington",
    IM_efficient = "Information Matrix (IM) Test",
    McCullagh_Test = "McCullagh Test",
    leCessie_Test = "leCessie Test",
    hosmer_bootstrap = "Hosmer Bootstrap (Lai & Liu)",
    bagoft_test_NO_SIM_Split_1 = "BaGofT (Split=1)",
    bagoft_test_Split_20 = "BaGofT (Split=20)",
    stute_zhu_test = "Stute Zhu (B=200)",
    PR_test = "PR Test",
    Final_Farrington_G4 = "Ebrahim-Farrington (G=4)",
    Final_Farrington_G40 = "Ebrahim-Farrington (G=40)",
    .default = df$Test
  )
  
  df$n_factor <- factor(df$n, levels = sort(unique(df$n)))
  
  # Create color labels for text based on background color
  df$text_color <- ifelse(df$Power >= 0.10, "white", 
                         ifelse(df$Power >= 0.03, "white", "white"))
  
  # Plot: Test vs n, fill = Power, facet by Scenario with CUSTOM TYPE I ERROR COLORS
  p <- ggplot(df, aes(x = n_factor, y = reorder(Test_clean, Power), fill = Power)) +
    geom_tile(color = "white", size = 0.5) +
    create_type1_color_scale() +  # Use custom color scale
    labs(
      title = plot_title,
      subtitle = "Type I Error rates across sample sizes (Target: ≤5%)",
      x = "Sample Size (n)",
      y = "Statistical Test"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40"),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 11, face = "bold"),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.position = "bottom",
      panel.grid = element_blank(),
      strip.text = element_text(size = 11, face = "bold")
    ) +
    geom_text(aes(label = ifelse(Power > 0, paste0(round(Power*100, 1), "%"), "0%")), 
              color = "white", size = 2.8, fontface = "bold") +
    facet_wrap(~ Scenario, ncol = 2, scales = "free_y") +
    # Add reference lines at 5% and 10% in the legend area (conceptually)
    annotate("text", x = Inf, y = Inf, 
             label = " ", 
             hjust = 1.05, vjust = 1.5, size = 3, color = "grey50", fontface = "italic")
  
  ggsave(output_png, plot = p, width = 12, height = 8, dpi = 300)
  cat("Saved Type I Error heat map with custom colors to", output_png, "\n")
}

# =============================================================================
# REGULAR POWER ANALYSIS PLOTTING FUNCTION - UPDATED WITH BETTER COLOR PALETTE
# =============================================================================

plot_summary_heatmap <- function(summary_csv, output_png, scenarios_filter = NULL, plot_title = "Power Analysis") {
  df <- read.csv(summary_csv, stringsAsFactors = FALSE)
  
  # Filter scenarios if specified
  if (!is.null(scenarios_filter)) {
    df <- df[df$Scenario %in% scenarios_filter, ]
  }
  
  # =============================================================================
  # APPLY TEST EXCLUSIONS FOR HEAT MAPS
  # =============================================================================
  # Remove tests that are in the exclusion list
  if (exists("tests_to_exclude_from_heatmaps") && length(tests_to_exclude_from_heatmaps) > 0) {
    original_test_count <- length(unique(df$Test))
    df <- df[!df$Test %in% tests_to_exclude_from_heatmaps, ]
    excluded_count <- original_test_count - length(unique(df$Test))
    
    if (excluded_count > 0) {
      cat(sprintf("Heat map generation: Excluded %d test(s) from visualization\n", excluded_count))
    }
  }
  
  # Check if we have any data left after exclusions
  if (nrow(df) == 0) {
    cat("Warning: No data remaining after applying exclusions. Skipping plot generation.\n")
    return(invisible(NULL))
  }
  
  # Clean up test names for display - HOSMER METHODOLOGY COMPARISON
  df$Test_clean <- dplyr::recode(df$Test,
    # PRIMARY TESTS FOR HOSMER METHODOLOGY COMPARISON
    Final_Farrington_G10 = "Ebrahim-Farrington (G=10)",
    xie = "Xie Test",
    bagoft_parallel = "BAGofT (Split=20, B=100)",
    Tsiatis_clustering = "Tsiatis Score Test", 
    traditional_HL = "Hosmer-Lemeshow (Equal Size)",
    hosmer_equal_width = "Hosmer-Lemeshow (Equal Width)",
    pigeonheyse = "Pigeon-Heyse Test",
    
    # OTHER TESTS (should be excluded but include mapping for completeness)
    giviti_calib = "GIVITI Calibration (external)",
    giviti_calib_internal = "GIVITI Calibration (internal)",
    pearson = "Pearson χ²",
    deviance = "Deviance",
    hl_ftest = "HL F-test",
    osius_rojek = "Osius-Rojek",
    s_st_pgeq0_5 = "Stukel ≥0.5",
    s_st_pl0_5 = "Stukel <0.5", 
    sst_both = "Stukel Both",
    large_s_hl = "Large Sample HL",
    spiegelhalter = "Spiegelhalter",
    Copas_unweighted_S = "Copas Unweighted",
    eHL = "eHL (20)",
    eHL_10 = "eHL (10)",
    U_p = "Unreliability Index",
    HL_GAM = "HL+GAM",
    PR_GAM = "PR+GAM",
    XIE_GAM = "XIE+GAM",
    Farrington_standalone = "Farrington",
    IM_efficient = "Information Matrix (IM) Test",
    McCullagh_Test = "McCullagh Test",
    leCessie_Test = "leCessie Test",
    hosmer_bootstrap = "Hosmer Bootstrap (Lai & Liu)",
    bagoft_test_NO_SIM_Split_1 = "BaGofT (Split=1)",
    bagoft_test_Split_20 = "BaGofT (Split=20)",
    stute_zhu_test = "Stute Zhu (B=200)",
    PR_test = "PR Test",
    Final_Farrington_G4 = "Ebrahim-Farrington (G=4)",
    Final_Farrington_G40 = "Ebrahim-Farrington (G=40)",
    .default = df$Test
  )
  
  df$n_factor <- factor(df$n, levels = sort(unique(df$n)))
  
  # Determine text color based on power level for better readability
  # Use white font if Power >= 0.6, or Power >= 0.3, or Power < 0.03; otherwise black
  df$text_color <- ifelse(df$Power >= 0.4 | df$Power < 0.03, "white", "#4d4d4d")
  
  # Plot: Test vs n, fill = Power, facet by Scenario - IMPROVED COLOR PALETTE FOR POWER
  p <- ggplot(df, aes(x = n_factor, y = reorder(Test_clean, Power), fill = Power)) +
    geom_tile(color = "white", size = 0.5) +
    # Custom power color scale: Dark Red (low) -> Orange -> Yellow -> Light Green -> Dark Green (high)
    scale_fill_gradientn(
      name = "Power",
      colors = c("#8B0000", "#CD3333", "#e3b47a", "#95d349", "#9ACD32", "#228B22", "#006400"),
      values = c(0, 0.02, 0.10, 0.2, 0.3, 0.65, 1.0),
      labels = scales::percent_format(accuracy = 1),
      na.value = "grey90",
      limits = c(0, 1),
      guide = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 15,
        barheight = 1.5
      )
    ) +
    labs(
      title = plot_title,
      subtitle = "Power to detect model misspecification across sample sizes (Higher is better)",
      x = "Sample Size (n)",
      y = "Statistical Test"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40"),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 11, face = "bold"),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.position = "bottom",
      panel.grid = element_blank(),
      strip.text = element_text(size = 11, face = "bold")
    ) +
    geom_text(aes(label = ifelse(Power > 0, paste0(round(Power*100, 1), "%"), "0%"), 
                  color = I(text_color)), 
              size = 2.8, fontface = "bold") +
    facet_wrap(~ Scenario, ncol = 2, scales = "free_y") +
    # Add reference annotation for power interpretation
    annotate("text", x = Inf, y = Inf, 
             label = " ", 
             hjust = 1.05, vjust = 1.5, size = 3, color = "grey50", fontface = "italic")
  
  ggsave(output_png, plot = p, width = 12, height = 8, dpi = 300)
  cat("Saved Power Analysis heat map with improved color palette to", output_png, "\n")
}

# =============================================================================
# GENERATE PLOTS WITH CUSTOM COLORS FOR TYPE I ERROR
# =============================================================================

output_dir <- "C:/Users/ebrah/.cursor-tutor/projects/"

# For Type 1 Error scenarios - use CUSTOM COLOR SCALE
type1_csv <- file.path(output_dir, "type1error_summary_alln.csv")
if (file.exists(type1_csv)) {
  # Plot 1: U(-3,3) and U(-6,6) together - WITH CUSTOM COLORS
  plot_type1_error_heatmap(
    type1_csv,
    file.path(output_dir, "type1error_uniform_scenarios_heatmap.png"),
    scenarios_filter = c("Uniform_-3_3", "Uniform_-6_6"),
    plot_title = "Type I Error Analysis - Uniform Distributions"
  )
  
  # Plot 2: Chi2 and Multi_Indep together - WITH CUSTOM COLORS
  plot_type1_error_heatmap(
    type1_csv,
    file.path(output_dir, "type1error_chi2_multi_scenarios_heatmap.png"),
    scenarios_filter = c("Chi2_4", "Multi_Indep"),
    plot_title = "Type I Error Analysis - Chi-squared and Multi-variable"
  )
  
  # Plot 3: Normal alone - WITH CUSTOM COLORS
  plot_type1_error_heatmap(
    type1_csv,
    file.path(output_dir, "type1error_normal_scenario_heatmap.png"),
    scenarios_filter = c("Normal_0_1.5"),
    plot_title = "Type I Error Analysis - Normal Distribution"
  )
  
  # Also create a combined plot for all Type I Error scenarios
  plot_type1_error_heatmap(
    type1_csv,
    file.path(output_dir, "type1error_summary_alln_heatmap.png"),
    scenarios_filter = NULL,  # Include all Type I scenarios
    plot_title = "Type I Error Analysis - All Scenarios"
  )
}

# For Power scenarios - keep original viridis colors
plot_summary_heatmap(
  file.path(output_dir, "power_summary_omitted_variable_alln.csv"),
  file.path(output_dir, "power_summary_omitted_variable_alln_heatmap.png"),
  plot_title = "Power Analysis - Omitted Variable Scenarios"
)
plot_summary_heatmap(
  file.path(output_dir, "power_summary_omitted_interaction_alln.csv"),
  file.path(output_dir, "power_summary_omitted_interaction_alln_heatmap.png"),
  plot_title = "Power Analysis - Omitted Interaction Scenarios"
)

cat("\n=== HEAT MAP GENERATION COMPLETED ===\n")
cat("✅ Type I Error heat maps: Custom color scheme (Dark Blue → Green → Red)\n")
cat("✅ Power analysis heat maps: Original viridis color scheme\n")
cat("✅ All plots saved to:", output_dir, "\n")