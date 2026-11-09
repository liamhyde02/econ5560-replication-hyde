# Step 4: Figure 1, attribute WTP by management regime, ours next to the paper's.
#
# Reads outputs/table4_wtp.csv (written by step 3) and draws the three attribute
# WTPs for Model 2 national and community with 95% intervals, beside the
# published Table 4 values. Writes outputs/figure1_wtp.png and .pdf.

w <- read.csv("outputs/table4_wtp.csv")
attrs <- c("fish_catch", "coral_coverage", "coastal_conditions")
labels <- c("Fish catch", "Coral coverage", "Coastal protection")
pick <- function(series, col) w[w$series == series, ][match(attrs, w$attribute[w$series == series]), col]

est <- rbind(pick("Model 2 national", "wtp_yen"), pick("Model 2 national", "paper_wtp"),
             pick("Model 2 community", "wtp_yen"), pick("Model 2 community", "paper_wtp"))
se <- rbind(pick("Model 2 national", "std_err"), pick("Model 2 national", "paper_se"),
            pick("Model 2 community", "std_err"), pick("Model 2 community", "paper_se"))
cols <- c("#176E73", "#A3C9CB", "#B56540", "#E5C3B3")
legend_text <- c("National (top-down), this replication", "National (top-down), paper Table 4",
                 "Community (bottom-up), this replication", "Community (bottom-up), paper Table 4")

draw <- function() {
  par(mar = c(4, 5, 3, 1))
  lo <- est - 1.96 * se
  hi <- est + 1.96 * se
  x <- barplot(est, beside = TRUE, col = cols, names.arg = labels, ylim = c(min(0, lo) - 0.5, max(hi) + 2.5),
               ylab = "WTP (yen per month per percentage point)",
               main = "Marginal WTP by management regime (Model 2), 95% intervals")
  arrows(x, lo, x, hi, angle = 90, code = 3, length = 0.04)
  abline(h = 0)
  legend("topleft", legend = legend_text, fill = cols, bty = "n", cex = 0.8)
}
png("outputs/figure1_wtp.png", width = 1200, height = 720, res = 150)
draw()
invisible(dev.off())
pdf("outputs/figure1_wtp.pdf", width = 8, height = 4.8)
draw()
invisible(dev.off())
cat("Wrote outputs/figure1_wtp.png and outputs/figure1_wtp.pdf\n")
