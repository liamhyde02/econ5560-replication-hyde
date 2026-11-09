# Step 3: Models 1 and 2 (Table 3) and willingness to pay (Table 4).
#
# Estimated with mlogit, using the team's specification (Zoey Miles,
# code/ReplicationRevised.qmd in the team repository) and the paper's Eqs. 1-2:
#   Model 1: fish_catch, coral_coverage, coastal_conditions, contribution (yen),
#            ASC; all independent normal random parameters.
#   Model 2: Model 1 plus each term interacted with community = (survey_type == 2),
#            also independent normal random parameters.
#   panel = TRUE (draws shared across a respondent's choices), R = 500 Halton draws.
#   Start values as in the team's code: Model 1 from the fixed-coefficient logit
#   with SDs at half the absolute coefficient; Model 2 from Model 1's means with
#   the interactions at 0, Model 1's SDs, and interaction SDs at half of those.
# Marginal WTP = -b_k / b_pay (the paper's Eq. 3 with its minus sign restored),
# yen per month per percentage point, delta-method standard errors. For the
# community group, WTP = -(b_k + g_k) / (b_pay + g_pay).
#
# Writes outputs/table3_coefficients.csv, outputs/table4_wtp.csv and
# outputs/model_summary.csv.

suppressPackageStartupMessages({
  library(mlogit)
  library(dfidx)
})

d <- as.data.frame(readxl::read_excel("data/raw/MPA_journal_data_with_choice_for_CalPolyFA2026.xlsx", sheet = "Data"))
base <- c("fish_catch", "coral_coverage", "coastal_conditions", "contribution", "ASC")
inter <- paste0(base, "_comm")

# Row order as in the team's code: by choice set, status quo last.
d <- d[order(d$Cid, -d$ASC, d$record_iidd), ]
d$alt <- ave(d$Cid, d$Cid, FUN = seq_along)
stopifnot(all(d$alt == d$Alt), all(tapply(d$Choice, d$Cid, sum) == 1))
d$choice <- as.logical(d$Choice)
d$community <- as.numeric(d$survey_type == 2)
for (i in seq_along(base)) d[[inter[i]]] <- d[[base[i]]] * d$community
md <- dfidx(d[, c("Cid", "record_id", "alt", "choice", base, inter)],
            idx = list(c("Cid", "record_id"), "alt"))

f1 <- as.formula(paste("choice ~", paste(base, collapse = " + "), "| -1"))
f2 <- as.formula(paste("choice ~", paste(c(base, inter), collapse = " + "), "| -1"))
rpar1 <- setNames(rep("n", 5), base)
rpar2 <- setNames(rep("n", 10), c(base, inter))
n_draws <- 500

m0 <- mlogit(f1, data = md)
b0 <- coef(m0)
cat("Fixed-coefficient logit log-likelihood:", round(as.numeric(logLik(m0)), 3), "\n")
m1 <- mlogit(f1, data = md, rpar = rpar1, R = n_draws, halton = NA, panel = TRUE,
             start = c(b0, abs(b0) / 2))
cat("Model 1 log-likelihood:", round(as.numeric(logLik(m1)), 3), "\n")
b1 <- coef(m1)
sds1 <- abs(b1[6:10])
m2 <- mlogit(f2, data = md, rpar = rpar2, R = n_draws, halton = NA, panel = TRUE,
             start = c(b1[1:5], rep(0, 5), sds1, sds1 / 2))
cat("Model 2 log-likelihood:", round(as.numeric(logLik(m2)), 3), "\n")
# Each model nests the one before it, so a lower log-likelihood means the optimizer stalled.
stopifnot(as.numeric(logLik(m1)) >= as.numeric(logLik(m0)) - 1e-6,
          as.numeric(logLik(m2)) >= as.numeric(logLik(m1)) - 1e-6)

stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))

# Shah et al. (2019) Table 3: published mean coefficients and significance.
paper3 <- data.frame(
  model = c(rep("Model 1", 5), rep("Model 2", 10)),
  term = c(base, base, inter),
  paper_estimate = c(0.0069, 0.0360, 0.0096, -0.0055, 8.220,
                     0.0035, 0.0299, 0.005, -0.0052, 8.461, 0.00758, 0.0168, 0.0105, -0.0011, 0.225),
  paper_sig = c("**", "***", "***", "***", "***", "", "***", "", "***", "***", "", "**", "*", "**", ""))

coef_table <- function(m, label) {
  b <- coef(m)
  se <- sqrt(diag(vcov(m)))
  p <- 2 * pnorm(-abs(b / se))
  data.frame(model = label, term = names(b), estimate = unname(b), std_err = unname(se),
             z = unname(b / se), p_value = unname(p), sig = stars(unname(p)))
}
t3 <- rbind(coef_table(m1, "Model 1"), coef_table(m2, "Model 2"))
t3 <- merge(t3, paper3, by = c("model", "term"), all.x = TRUE, sort = FALSE)
t3 <- t3[order(t3$model, match(t3$term, names(coef(m2)))), ]
write.csv(t3, "outputs/table3_coefficients.csv", row.names = FALSE)

wtp <- function(m, num, den) {
  b <- coef(m)
  a <- sum(b[num]); c <- sum(b[den])
  g <- setNames(numeric(length(b)), names(b))
  g[num] <- -1 / c
  g[den] <- g[den] + a / c^2
  c(est = -a / c, se = sqrt(drop(t(g) %*% vcov(m) %*% g)))
}
# Shah et al. (2019) Table 4: published WTP and standard errors.
paper4 <- list(
  "Model 1 (pooled)" = list(fish_catch = c(1.244, 0.503), coral_coverage = c(6.524, 0.616),
                            coastal_conditions = c(1.744, 0.530), ASC = c(1489.0, 147.8)),
  "Model 2 national" = list(fish_catch = c(0.677, 0.680), coral_coverage = c(5.766, 0.821),
                            coastal_conditions = c(0.958, 0.734), ASC = c(1631.7, 161.8)),
  "Model 2 community" = list(fish_catch = c(1.754, 0.745), coral_coverage = c(7.383, 0.920),
                             coastal_conditions = c(2.447, 0.766), ASC = c(1373.8, 137.5)))
rows <- list()
for (a in c("fish_catch", "coral_coverage", "coastal_conditions", "ASC")) {
  specs <- list(list("Model 1 (pooled)", m1, a, "contribution"),
                list("Model 2 national", m2, a, "contribution"),
                list("Model 2 community", m2, c(a, paste0(a, "_comm")), c("contribution", "contribution_comm")))
  for (s in specs) {
    w <- wtp(s[[2]], s[[3]], s[[4]])
    p <- 2 * pnorm(-abs(w[["est"]] / w[["se"]]))
    rows[[length(rows) + 1]] <- data.frame(
      series = s[[1]], attribute = a, wtp_yen = w[["est"]], std_err = w[["se"]], p_value = p, sig = stars(p),
      paper_wtp = paper4[[s[[1]]]][[a]][1], paper_se = paper4[[s[[1]]]][[a]][2])
  }
}
t4 <- do.call(rbind, rows)
write.csv(t4, "outputs/table4_wtp.csv", row.names = FALSE)

summ <- data.frame(
  item = c("respondents", "choice_occasions", "halton_draws", "loglik_fixed_logit", "loglik_model1", "loglik_model2",
           "iterations_model1", "iterations_model2", "gradient_criterion_model1", "gradient_criterion_model2"),
  value = c(length(unique(d$record_id)), length(unique(d$Cid)), n_draws, as.numeric(logLik(m0)),
            as.numeric(logLik(m1)), as.numeric(logLik(m2)), m1$est.stat$nb.iter, m2$est.stat$nb.iter,
            signif(m1$est.stat$eps, 3), signif(m2$est.stat$eps, 3)))
write.csv(summ, "outputs/model_summary.csv", row.names = FALSE)

print(summary(m1))
print(summary(m2))
num <- vapply(t3, is.numeric, logical(1))
t3p <- t3; t3p[num] <- lapply(t3p[num], round, 4)
print(t3p, row.names = FALSE)
num <- vapply(t4, is.numeric, logical(1))
t4p <- t4; t4p[num] <- lapply(t4p[num], round, 3)
print(t4p, row.names = FALSE)
