# =============================================================================
# EXCLUDED TESTS PAPER FIGURES
# =============================================================================
# Purpose: Generate 9 line chart figures for excluded tests
# Layout: 5 Type I Error figures (top row) + 4 Power figures (bottom row)
# Format: A4 PNG figures with multiple lines per test
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(gridExtra)
library(grid)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Output directory - Changed to Proposal Tex folder
output_dir <- "C:/Users/ebrah/.cursor-tutor/Proposal Tex/"

# Tests to include (from exclusion list)
excluded_tests_to_plot <- c(
  "Farrington_standalone", 
  "hl_ftest", 
  "PR_GAM", 
  "U_p", 
  "pearson", 
  "deviance", 
  "PR_test", 
  "hosmer_bootstrap"
)

# Scenarios configuration
type1_scenarios <- c("Uniform_-3_3", "Uniform_-6_6", "Normal_0_1.5", "Chi2_4", "Multi_Indep")
power_scenarios <- c("Quad_Slight", "Quad_Pronounced", "Interact_Slight", "Interact_Pronounced")

# Test name mapping for display
test_display_names <- c(
  "Farrington_standalone" = "Farrington",
  "hl_ftest" = "HL F-test",
  "PR_GAM" = "PR+GAM",
  "U_p" = "Unreliability Index",
  "pearson" = "Pearson χ²",
  "deviance" = "Deviance",
  "PR_test" = "PR Test",
  "hosmer_bootstrap" = "Hosmer Bootstrap"
)

# Custom color palette - Using a distinctive, colorblind-friendly palette
custom_colors <- c(
  "#E31A1C",  # Red
  "#1F78B4",  # Blue  
  "#33A02C",  # Green
  "#FF7F00",  # Orange
  "#76001b",  # Purple
  "#B15928",  # Brown
  "#A6CEE3",  # Light Blue
  "#FDBF6F"   # Light Orange
)

# Scenario display names
scenario_display_names <- c(
  "Uniform_-3_3" = "Uniform(-3,3)",
  "Uniform_-6_6" = "Uniform(-6,6)", 
  "Normal_0_1.5" = "Normal(0,1.5)",
  "Chi2_4" = "Chi-squared(4)",
  "Multi_Indep" = "Multi-Independent",
  "Quad_Slight" = "Quadratic (Slight)",
  "Quad_Pronounced" = "Quadratic (Pronounced)",
  "Interact_Slight" = "Interaction (Slight)",
  "Interact_Pronounced" = "Interaction (Pronounced)"
)

# =============================================================================
# DATA LOADING AND PROCESSING FUNCTION
# =============================================================================

load_and_process_data <- function(scenario_name, data_dir) {
  # Find all CSV files for this scenario - Look in projects folder for data
  projects_dir <- "C:/Users/ebrah/.cursor-tutor/projects/"
  pattern <- paste0("^pvalues_", scenario_name, "_n[0-9]+\\.csv$")
  files <- list.files(projects_dir, pattern = pattern, full.names = TRUE)
  
  if (length(files) == 0) {
    cat("Warning: No files found for scenario:", scenario_name, "\n")
    return(NULL)
  }
  
  # Process each file
  all_data <- list()
  
  for (file in files) {
    # Extract sample size from filename
    n_match <- regexec("_n([0-9]+)\\.csv$", basename(file))
    n_str <- regmatches(basename(file), n_match)[[1]][2]
    n_size <- as.numeric(n_str)
    
    # Read data
    data <- read.csv(file, stringsAsFactors = FALSE)
    
    # Filter for excluded tests only
    available_tests <- intersect(excluded_tests_to_plot, names(data))
    if (length(available_tests) == 0) next
    
    # Calculate rejection rates for each test
    for (test in available_tests) {
      pvals <- as.numeric(as.character(unlist(data[[test]])))
      pvals <- pvals[!is.na(pvals)]
      
      if (length(pvals) > 0) {
        rejection_rate <- mean(pvals < 0.05, na.rm = TRUE) * 100
        
        all_data[[paste(scenario_name, test, n_size, sep = "_")]] <- data.frame(
          Scenario = scenario_name,
          Test = test,
          Test_Display = test_display_names[test],
          n = n_size,
          Rejection_Rate = rejection_rate,
          n_replications = length(pvals),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(all_data) > 0) {
    return(do.call(rbind, all_data))
  } else {
    return(NULL)
  }
}

# =============================================================================
# PLOTTING FUNCTION
# =============================================================================

create_line_plot <- function(scenario_data, plot_title, y_label = "Rejection Rate (%)", show_legend = FALSE, show_reference_lines = FALSE) {
  if (is.null(scenario_data) || nrow(scenario_data) == 0) {
    # Create empty plot with message
    return(ggplot() + 
           annotate("text", x = 0.5, y = 0.5, label = "No data available", size = 8) +
           labs(title = plot_title) +
           theme_void() +
           theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))
  }
  
  # Ensure n is treated as numeric for proper ordering
  scenario_data$n <- as.numeric(scenario_data$n)
  scenario_data <- scenario_data[order(scenario_data$n), ]
  
  # Add slight jitter to x values to prevent perfect overlap
  scenario_data$n_jittered <- scenario_data$n + 
    as.numeric(as.factor(scenario_data$Test_Display)) * 5 - 25  # Small offset for each test
  
  # Add slight vertical offset to zero values to make them visible
  scenario_data$Rejection_Rate_adjusted <- ifelse(
    scenario_data$Rejection_Rate == 0,
    0.2 + as.numeric(as.factor(scenario_data$Test_Display)) * 0.1,  # Slightly above zero with stacking
    scenario_data$Rejection_Rate
  )
  
  # Ensure all tests from excluded_tests_to_plot are represented (even if missing data)
  # Create a complete data frame with all combinations
  all_n_values <- unique(scenario_data$n)
  all_tests <- excluded_tests_to_plot
  all_test_displays <- test_display_names[all_tests]
  
  # Create complete grid
  complete_grid <- expand.grid(
    n = all_n_values,
    Test = all_tests,
    stringsAsFactors = FALSE
  )
  complete_grid$Test_Display <- test_display_names[complete_grid$Test]
  
  # Merge with actual data
  scenario_data_complete <- merge(complete_grid, scenario_data, 
                                  by = c("n", "Test", "Test_Display"), all.x = TRUE)
  
  # Fill missing values with 0 and apply adjustments
  scenario_data_complete$Rejection_Rate[is.na(scenario_data_complete$Rejection_Rate)] <- 0
  scenario_data_complete$n_jittered <- scenario_data_complete$n + 
    as.numeric(as.factor(scenario_data_complete$Test_Display)) * 5 - 25
  
  scenario_data_complete$Rejection_Rate_adjusted <- ifelse(
    scenario_data_complete$Rejection_Rate == 0,
    0.2 + as.numeric(as.factor(scenario_data_complete$Test_Display)) * 0.1,
    scenario_data_complete$Rejection_Rate
  )
  
  # Use the complete data
  scenario_data <- scenario_data_complete
  
  # Determine y-axis limits to ensure reference lines are visible
  if (show_reference_lines) {
    y_max <- max(scenario_data$Rejection_Rate_adjusted, 12) # At least 12% to show both reference lines
  } else {
    y_max <- max(scenario_data$Rejection_Rate_adjusted, 5) # Just above data range
  }
  
  # Create the plot with dotted lines and custom colors
  p <- ggplot(scenario_data, aes(x = n_jittered, y = Rejection_Rate_adjusted, color = Test_Display, group = Test_Display)) +
    # Main data lines and points - All lines as dotted with transparency
    geom_line(linetype = "dotted", size = 2.5, alpha = 0.75) +
    geom_point(size = 4.5, alpha = 0.85) +
    # Use custom color palette with all tests included
    scale_color_manual(name = "Statistical Test", values = custom_colors, drop = FALSE) +
    scale_x_continuous(
      name = "Sample Size (n)",
      breaks = unique(scenario_data$n),
      labels = unique(scenario_data$n)
    ) +
    scale_y_continuous(
      name = y_label,
      labels = function(x) {
        # Custom labeling to show 0% for adjusted values near zero
        ifelse(x < 1, "0%", paste0(round(x), "%"))
      },
      limits = c(0, y_max * 1.05),
      breaks = if(show_reference_lines) c(0, 5, 10, seq(20, 100, 20)) else waiver()
    ) +
    labs(title = plot_title) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 8)),
      axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 8)),
      axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 8)),
      axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 11),
      legend.position = if(show_legend) "bottom" else "none",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.8, "cm"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", size = 0.4),
      panel.grid.major.y = element_line(color = "grey90", size = 0.4),
      plot.margin = margin(8, 8, 8, 8)
    )
  
  # Add reference lines only for Type I Error figures (show_reference_lines = TRUE)
  if (show_reference_lines) {
    p <- p +
      # Add reference lines for 5% and 10% significance levels in dark gray
      geom_hline(yintercept = 5, color = "#555555", linetype = "solid", size = 1, alpha = 0.8) +
      geom_hline(yintercept = 10, color = "#555555", linetype = "solid", size = 1, alpha = 0.8) +
      # Add reference line labels in dark gray
      annotate("text", x = Inf, y = 5, label = "5%", hjust = 1.1, vjust = -0.3, 
               color = "#555555", size = 3.5, fontface = "bold") +
      annotate("text", x = Inf, y = 10, label = "10%", hjust = 1.1, vjust = -0.3, 
               color = "#555555", size = 3.5, fontface = "bold")
  }
  
  if (show_legend) {
    p <- p + guides(
      color = guide_legend(
        nrow = 2, 
        byrow = TRUE, 
        title.position = "top",
        override.aes = list(size = 3, alpha = 1, linetype = "dotted")
      )
    )
  }
  
  return(p)
}

# =============================================================================
# FUNCTION TO EXTRACT LEGEND
# =============================================================================

extract_legend <- function(plot) {
  # Extract legend from a ggplot object using cowplot
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

cat("=== GENERATING EXCLUDED TESTS PAPER FIGURES ===\n")
cat("Loading and processing data for 9 scenarios...\n\n")

# Load required library for legend extraction
if (!require(cowplot, quietly = TRUE)) {
  install.packages("cowplot")
  library(cowplot)
}

# Load data for all scenarios
all_plots <- list()
shared_legend <- NULL

# Type I Error scenarios (5 figures) - WITH reference lines
cat("Processing Type I Error scenarios:\n")
for (i in seq_along(type1_scenarios)) {
  scenario <- type1_scenarios[i]
  cat(sprintf("  %d. %s\n", i, scenario))
  
  scenario_data <- load_and_process_data(scenario, output_dir)
  display_name <- scenario_display_names[scenario]
  
  # Remove "Type I Error:" from title since it's in section header
  plot_title <- display_name
  
  # Create plot with legend only for first valid plot to extract it
  show_legend <- (i == 1 && !is.null(scenario_data) && nrow(scenario_data) > 0)
  
  plot <- create_line_plot(
    scenario_data, 
    plot_title,
    "Type I Error Rate (%)",
    show_legend = show_legend,
    show_reference_lines = TRUE  # Show 5% and 10% lines for Type I Error
  )
  
  # Extract legend from first valid plot with data
  if (show_legend && is.null(shared_legend)) {
    # Create a temporary plot with larger legend for extraction
    temp_plot <- plot + theme(
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      legend.key.width = unit(2.5, "cm"),
      legend.key.height = unit(1, "cm")
    )
    shared_legend <- extract_legend(temp_plot)
    # Remove legend from this plot
    plot <- plot + theme(legend.position = "none")
  }
  
  all_plots[[paste0("type1_", i)]] <- plot
}

# Power scenarios (4 figures) - WITHOUT reference lines
cat("\nProcessing Power scenarios:\n")
for (i in seq_along(power_scenarios)) {
  scenario <- power_scenarios[i]
  cat(sprintf("  %d. %s\n", i, scenario))
  
  scenario_data <- load_and_process_data(scenario, output_dir)
  display_name <- scenario_display_names[scenario]
  
  # Remove "Power Analysis:" from title since it's in section header  
  plot_title <- display_name
  
  plot <- create_line_plot(
    scenario_data, 
    plot_title,
    "Power (%)",
    show_legend = FALSE,
    show_reference_lines = FALSE  # NO reference lines for Power scenarios
  )
  
  all_plots[[paste0("power_", i)]] <- plot
}

# =============================================================================
# ARRANGE AND SAVE FIGURES
# =============================================================================

cat("\n=== CREATING PAPER LAYOUT ===\n")

# A4 Landscape dimensions: 11.69 x 8.27 inches
fig_width <- 23.38  # A4 landscape width * 2 for better resolution  
fig_height <- 16.54 # A4 landscape height * 2 for better resolution

# Arrange plots
cat("Arranging figures in 5+4 landscape layout...\n")

# Top row: 5 Type I Error figures
top_row <- do.call(grid.arrange, c(
  all_plots[paste0("type1_", 1:5)], 
  list(nrow = 1)
))

# Bottom row: 4 Power figures with spacing
bottom_row_plots <- all_plots[paste0("power_", 1:4)]
# Add empty plot for spacing to center the 4 plots
empty_plot <- ggplot() + theme_void()
bottom_row <- grid.arrange(
  empty_plot, bottom_row_plots[[1]], bottom_row_plots[[2]], 
  bottom_row_plots[[3]], bottom_row_plots[[4]], empty_plot,
  nrow = 1, widths = c(0.5, 1, 1, 1, 1, 0.5)
)

# Create section titles with larger fonts
type1_title <- textGrob("Type I Error Analysis", 
                       gp = gpar(fontsize = 18, fontface = "bold"))
power_title <- textGrob("Power Analysis", 
                       gp = gpar(fontsize = 18, fontface = "bold"))

# Main title with larger font
main_title <- textGrob("Excluded Tests Performance Analysis", 
                      gp = gpar(fontsize = 22, fontface = "bold"))

# Combine all elements with shared legend
if (!is.null(shared_legend)) {
  final_layout <- grid.arrange(
    main_title,
    type1_title,
    top_row,
    power_title, 
    bottom_row,
    shared_legend,
    nrow = 6,
    heights = c(1, 0.5, 4, 0.5, 4, 1.2)  # More space for legend
  )
} else {
  # Fallback if no legend was extracted
  cat("Warning: No legend was extracted. Creating layout without shared legend.\n")
  final_layout <- grid.arrange(
    main_title,
    type1_title,
    top_row,
    power_title, 
    bottom_row,
    nrow = 5,
    heights = c(1, 0.5, 4, 0.5, 4)
  )
}

# Save the complete figure
output_file <- file.path(output_dir, "excluded_tests_paper_9_figures.png")
cat("Saving complete paper figure (A4 Landscape)...\n")

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

ggsave(
  filename = output_file,
  plot = final_layout,
  width = fig_width,
  height = fig_height,
  units = "in",
  dpi = 300,
  bg = "white"
)

cat("✅ Paper figure saved:", output_file, "\n")

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("\n=== SUMMARY ===\n")
cat("Generated 9 line chart figures:\n")
cat("  • 5 Type I Error scenarios:", paste(type1_scenarios, collapse = ", "), "\n")
cat("  • 4 Power scenarios:", paste(power_scenarios, collapse = ", "), "\n")
cat("  • Tests included:", paste(excluded_tests_to_plot, collapse = ", "), "\n")
cat("  • Output format: A4 Landscape PNG (300 DPI)\n")
cat("  • Output location:", output_file, "\n")
cat("  • Layout: 5×1 (top) + 4×1 (bottom) + shared legend\n")
cat("  • Features: Dotted lines, custom color palette, 5%/10% reference lines\n")
cat("\n=== COMPLETE ===\n") 