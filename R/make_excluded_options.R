suppressMessages({library(ggplot2); library(dplyr); library(scales); library(viridisLite)})
P   <- "C:/Users/ebrah/.cursor-tutor/projects"
OUT <- "C:/Users/ebrah/.gemini/Projects/PDFs/_thesis_paper/figures"
NS  <- c(200,500,1000,2000,5000)
scen_lab <- c("Uniform_-3_3"="Uniform(-3, 3)","Uniform_-6_6"="Uniform(-6, 6)","Normal_0_1.5"="Normal(0, 1.5)",
  "Chi2_4"="Chi-squared(4)","Multi_Indep"="Multi-Independent","Quad_Slight"="Quadratic (slight)",
  "Quad_Pronounced"="Quadratic (pronounced)","Interact_Slight"="Interaction (slight)","Interact_Pronounced"="Interaction (pronounced)")
excl_lab <- c(pearson="Pearson chi-square", deviance="Deviance", hl_ftest="HL F-test",
  hosmer_bootstrap="Hosmer bootstrap (Lai-Liu)", Farrington_standalone="Farrington",
  PR_test="Pulkstenis-Robinson", PR_GAM="PR+GAM", U_p="Unreliability index")
load1 <- function(csv, scen){
  d <- read.csv(file.path(P, csv), stringsAsFactors=FALSE); d$Power <- suppressWarnings(as.numeric(d$Power))
  d[d$Test %in% names(excl_lab) & d$Scenario %in% scen & d$n %in% NS, ]
}
d <- rbind(
  load1("type1error_summary_alln.csv", c("Uniform_-3_3","Uniform_-6_6","Normal_0_1.5","Chi2_4","Multi_Indep")),
  load1("power_summary_omitted_variable_alln.csv",    c("Quad_Slight","Quad_Pronounced")),
  load1("power_summary_omitted_interaction_alln.csv", c("Interact_Slight","Interact_Pronounced")))
ord <- c("Uniform_-3_3","Uniform_-6_6","Normal_0_1.5","Chi2_4","Multi_Indep",
         "Quad_Slight","Quad_Pronounced","Interact_Slight","Interact_Pronounced")
d$panel <- factor(scen_lab[d$Scenario], levels = scen_lab[ord])
d$nf    <- factor(d$n, levels = NS); d$rate <- d$Power*100
d$Test_clean <- factor(excl_lab[d$Test], levels = unname(excl_lab))

## ---------- Option 1: clean lines, NO dodge, transparency ----------
p1 <- ggplot(d, aes(nf, rate, color=Test_clean, group=Test_clean, shape=Test_clean)) +
  geom_hline(yintercept=c(5,10), color="grey75", linetype="dashed", linewidth=0.4) +
  geom_line(linewidth=1.0, alpha=0.55) +
  geom_point(size=2.4, alpha=0.9) +
  facet_wrap(~panel, ncol=5) +
  scale_color_brewer(palette="Dark2", name="Excluded test") +
  scale_shape_manual(values=c(16,17,15,18,16,17,15,18), name="Excluded test") +
  labs(title="Excluded tests: rejection rate versus sample size",
       subtitle="Top row: Type I error (target 5%); bottom row: power. Semi-transparent lines; Farrington & Unreliability sit at ~0% throughout.",
       x="Sample size (n)", y="Rejection rate (%)") +
  theme_minimal(base_size=15) +
  theme(legend.position="bottom",
    plot.title=element_text(size=18,face="bold",hjust=0.5), plot.subtitle=element_text(size=12,hjust=0.5,color="grey35"),
    axis.text.x=element_text(size=11,angle=45,hjust=1,color="black"), axis.text.y=element_text(size=12,color="black"),
    axis.title=element_text(size=13,face="bold"), strip.text=element_text(size=13,face="bold"),
    legend.text=element_text(size=12.5), legend.title=element_text(size=13.5,face="bold"), panel.grid.minor=element_blank()) +
  guides(color=guide_legend(nrow=2, override.aes=list(linewidth=1.4,size=3,alpha=1)), shape=guide_legend(nrow=2))
ggsave(file.path(OUT,"fig_excluded_lines.png"), p1, width=15, height=8.5, dpi=300, bg="white")
cat("saved fig_excluded_lines.png\n")

## ---------- Option 2: heatmap (no overlap possible) ----------
p2 <- ggplot(d, aes(nf, Test_clean, fill=rate)) +
  geom_tile(color="white", linewidth=0.5) +
  geom_text(aes(label=ifelse(is.na(rate),"",paste0(round(rate),"%"))),
            color=ifelse(!is.na(d$rate)&d$rate>55,"white","grey15"), size=2.5) +
  facet_wrap(~panel, ncol=5) +
  scale_fill_viridis_c(name="Rejection rate (%)", limits=c(0,100), na.value="grey90",
                       guide=guide_colorbar(title.position="top", title.hjust=0.5, barwidth=16, barheight=1.2)) +
  scale_y_discrete(limits=rev) +
  labs(title="Excluded tests: rejection rate versus sample size",
       subtitle="Top row: Type I error (a high rate is bad); bottom row: power (a high rate is good).",
       x="Sample size (n)", y=NULL) +
  theme_minimal(base_size=14) +
  theme(legend.position="bottom",
    plot.title=element_text(size=18,face="bold",hjust=0.5), plot.subtitle=element_text(size=12,hjust=0.5,color="grey35"),
    axis.text.x=element_text(size=10,angle=45,hjust=1,color="black"), axis.text.y=element_text(size=10.5,color="black"),
    axis.title=element_text(size=13,face="bold"), strip.text=element_text(size=12,face="bold"),
    legend.text=element_text(size=11), legend.title=element_text(size=12.5,face="bold"), panel.grid=element_blank())
ggsave(file.path(OUT,"fig_excluded_heatmap.png"), p2, width=15, height=8.5, dpi=300, bg="white")
cat("saved fig_excluded_heatmap.png\n")
