suppressMessages(library(rms))
d <- read.csv("C:/Users/ebrah/.cursor-tutor/projects/SA_Data.csv")

diag <- function(form){
  m <- glm(form, data = d, family = binomial); p <- fitted(m)
  v <- val.prob(p, d$y, pl = FALSE)
  list(p = p, C = as.numeric(v["C (ROC)"]), Brier = as.numeric(v["Brier"]))
}
a <- diag(y ~ age + lwt + race2 + race3 + smoke)                               # paper's Table 3 / Fig 6 model (no interactions)
b <- diag(y ~ age + lwt + race2 + race3 + smoke + age:lwt + smoke:lwt)         # with interactions (reference)
cat(sprintf("no-interaction  (paper Table 3 model): C=%.4f  Brier=%.4f\n", a$C, a$Brier))
cat(sprintf("with-interaction (reference)         : C=%.4f  Brier=%.4f\n", b$C, b$Brier))

pick <- a   # the paper's Fig 6 is the misspecified model WITHOUT interactions
cat(sprintf("USING no-interaction model -> C=%.4f Brier=%.4f (paper quotes C~0.69 / Brier~0.19)\n",
            pick$C, pick$Brier))

graphics.off()
out <- "C:/Users/ebrah/.gemini/Projects/PDFs/_thesis_paper/figures/fig_reliability_clean.png"
png(out, width = 10, height = 8, units = "in", res = 300, bg = "white")
par(mar = c(5, 5, 2, 2))
val.prob(pick$p, d$y, pl = TRUE, statloc = FALSE, legendloc = c(0.50, 0.27),
         smooth = TRUE, logistic.cal = TRUE, cex = 1.25)
dev.off()
cat("saved:", out, "\n")
