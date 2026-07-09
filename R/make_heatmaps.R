suppressMessages({library(ggplot2); library(dplyr); library(scales)})
P   <- "C:/Users/ebrah/.cursor-tutor/projects"
OUT <- "C:/Users/ebrah/.gemini/Projects/PDFs/_thesis_paper/figures"

## --- exact 25-test set shown in the current SLADS figures, with clean labels ---
lab <- c(
  bagoft_test_NO_SIM_Split_1 = "BAGofT (split=1)",
  leCessie_Test              = "le Cessie",
  sst_both                   = "Stukel (both)",
  McCullagh_Test             = "McCullagh",
  osius_rojek                = "Osius-Rojek",
  Tsiatis_clustering         = "Tsiatis (score)",
  Copas_unweighted_S         = "Copas (unweighted)",
  XIE_GAM                    = "Xie+GAM",
  giviti_calib_internal      = "GiViTI (internal)",
  IM_efficient               = "Information matrix",
  stute_zhu_test             = "Stute-Zhu (B=200)",
  traditional_HL             = "Hosmer-Lemeshow (C)",
  hosmer_equal_width         = "Hosmer-Lemeshow (H)",
  large_s_hl                 = "Large-sample HL",
  s_st_pgeq0_5               = "Stukel (>=0.5)",
  bagoft_parallel            = "BAGofT (split=20, B=100)",
  s_st_pl0_5                 = "Stukel (<0.5)",
  HL_GAM                     = "HL+GAM",
  pigeonheyse                = "Pigeon-Heyse",
  xie                        = "Xie",
  eHL_10                     = "eHL (10)",
  eHL                        = "eHL (20)",
  giviti_calib               = "GiViTI (external)",
  bagoft_test_Split_20       = "BAGofT (split=20)",
  spiegelhalter              = "Spiegelhalter")

scen_lab <- c(
  "Uniform_-3_3"        = "Uniform(-3, 3)",
  "Uniform_-6_6"        = "Uniform(-6, 6)",
  "Chi2_4"              = "Chi-squared(4)",
  "Multi_Indep"         = "Multi-Independent",
  "Normal_0_1.5"        = "Normal(0, 1.5)",
  "Quad_Pronounced"     = "Quadratic (pronounced)",
  "Quad_Slight"         = "Quadratic (slight)",
  "Interact_Pronounced" = "Interaction (pronounced)",
  "Interact_Slight"     = "Interaction (slight)")

NS <- c(200, 500, 1000, 2000, 5000)

prep <- function(csv, scen){
  d <- read.csv(file.path(P, csv), stringsAsFactors = FALSE)
  d$Power <- suppressWarnings(as.numeric(d$Power))
  d <- d[d$Test %in% names(lab) & d$Scenario %in% scen & d$n %in% NS, ]
  d$Test_clean <- factor(lab[d$Test])
  d$Scenario   <- factor(scen_lab[d$Scenario], levels = scen_lab[scen])
  d$n_factor   <- factor(d$n, levels = NS)
  d
}

base_theme <- theme_minimal(base_size = 13) + theme(
  plot.title    = element_text(size = 15, face = "bold", hjust = 0.5),
  plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey35"),
  axis.text.x   = element_text(size = 11, angle = 45, hjust = 1, color = "black"),
  axis.text.y   = element_text(size = 10.5, color = "black"),
  axis.title    = element_text(size = 12, face = "bold"),
  legend.position = "bottom",
  legend.title  = element_text(size = 11, face = "bold"),
  legend.text   = element_text(size = 10),
  panel.grid    = element_blank(),
  strip.text    = element_text(size = 12.5, face = "bold"),
  plot.background  = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA))

lbl <- function(x) ifelse(x > 0, paste0(round(x*100,1), "%"), "0%")

## ---------- TYPE I (fixed diverging scale centered at 5%) ----------
plot_type1 <- function(csv, scen, out){
  d <- prep(csv, scen)
  p <- ggplot(d, aes(n_factor, reorder(Test_clean, Power), fill = Power)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = lbl(Power)), color = "white", size = 2.9, fontface = "bold") +
    scale_fill_gradientn(
      name   = "Type I error rate",
      colors = c("#4575B4", "#4575B4", "#1A9850", "#FDAE61", "#D73027", "#D73027"),
      values = scales::rescale(c(0, 0.03, 0.05, 0.08, 0.12, 0.15), to = c(0,1)),
      limits = c(0, 0.15), oob = scales::squish,
      breaks = c(0, 0.025, 0.05, 0.075, 0.10, 0.15),
      labels = scales::percent_format(accuracy = 1), na.value = "grey90",
      guide  = guide_colorbar(title.position = "top", title.hjust = 0.5,
                              barwidth = 16, barheight = 1.3)) +
    facet_wrap(~ Scenario, ncol = 2, scales = "free_y") +
    labs(title = "Type I error rate across sample sizes (target: 5%)",
         subtitle = "Blue = over-conservative, green = nominal, red = liberal",
         x = "Sample size (n)", y = NULL) + base_theme
  ggsave(file.path(OUT, out), p, width = 12, height = 8, dpi = 300, bg = "white")
  cat("saved", out, "\n")
}

## ---------- POWER (red -> green over 0-100%, already appropriate) ----------
plot_power <- function(csv, scen, out){
  d <- prep(csv, scen)
  d$tcol <- ifelse(d$Power >= 0.45 | d$Power < 0.05, "white", "grey15")
  p <- ggplot(d, aes(n_factor, reorder(Test_clean, Power), fill = Power)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = lbl(Power), color = I(tcol)), size = 2.9, fontface = "bold") +
    scale_fill_gradientn(
      name   = "Power",
      colors = c("#8B0000","#CD3333","#E3B47A","#95D349","#228B22","#006400"),
      values = scales::rescale(c(0, 0.05, 0.15, 0.35, 0.7, 1.0), to = c(0,1)),
      limits = c(0, 1), labels = scales::percent_format(accuracy = 1), na.value = "grey90",
      guide  = guide_colorbar(title.position = "top", title.hjust = 0.5,
                              barwidth = 16, barheight = 1.3)) +
    facet_wrap(~ Scenario, ncol = 2, scales = "free_y") +
    labs(title = "Power to detect misspecification across sample sizes (higher is better)",
         subtitle = NULL, x = "Sample size (n)", y = NULL) + base_theme
  ggsave(file.path(OUT, out), p, width = 12, height = 8, dpi = 300, bg = "white")
  cat("saved", out, "\n")
}

plot_type1("type1error_summary_alln.csv", c("Uniform_-3_3","Uniform_-6_6"), "fig_type1_uniform_clean.png")
plot_type1("type1error_summary_alln.csv", c("Chi2_4","Multi_Indep"),        "fig_type1_chi2multi_clean.png")
plot_power("power_summary_omitted_variable_alln.csv",    c("Quad_Pronounced","Quad_Slight"),        "fig_power_quadratic_clean.png")
plot_power("power_summary_omitted_interaction_alln.csv", c("Interact_Pronounced","Interact_Slight"),"fig_power_interaction_clean.png")

## verification spot-checks against the published figures
v <- prep("type1error_summary_alln.csv", "Uniform_-3_3")
chk <- function(t,n) v$Power[v$Test_clean==t & v$n==n]
cat(sprintf("VERIFY U(-3,3) n=200: McCullagh=%.1f%% (fig 5.4)  Osius-Rojek=%.1f%% (fig 7.2)  Spiegelhalter=%.1f%% (fig 0)\n",
            chk("McCullagh",200)*100, chk("Osius-Rojek",200)*100, chk("Spiegelhalter",200)*100))
