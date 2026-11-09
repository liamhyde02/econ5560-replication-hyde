# Step 3: Models 1 and 2 (Table 3) and willingness to pay (Table 4).
#
# Specification, as in the team's mlogit code (Zoey Miles, ReplicationRevised.qmd)
# and the paper's Eqs. 1-2:
#   Model 1: fish_catch, coral_coverage, coastal_conditions, contribution (yen),
#            ASC; all independent normal random parameters.
#   Model 2: Model 1 plus each term interacted with community = (survey_type == 2),
#            also normal random parameters.
#   Panel by respondent, 500 Halton draws, simulated maximum likelihood.
#   Start values: Model 1 from the fixed-coefficient logit with SDs at half the
#   absolute coefficient; Model 2 from Model 1 with the interactions at 0.
# Marginal WTP = -b_k / b_pay (the paper's Eq. 3 with its minus sign restored),
# yen per month per percentage point, delta-method standard errors. For the
# community group, WTP = -(b_k + g_k) / (b_pay + g_pay).
#
# Writes outputs/table3_coefficients.csv, outputs/table4_wtp.csv and
# outputs/model_summary.csv.

source("code/mixed_logit_functions.R")

d <- as.data.frame(readxl::read_excel("data/raw/MPA_journal_data_with_choice_for_CalPolyFA2026.xlsx", sheet = "Data"))
d$community <- as.integer(d$survey_type == 2)
base <- c("fish_catch", "coral_coverage", "coastal_conditions", "contribution", "ASC")
inter <- paste0(base, "_comm")
for (i in seq_along(base)) d[[inter[i]]] <- d[[base[i]]] * d$community

md1 <- prepare_data(d, base)
md2 <- prepare_data(d, c(base, inter))

m0 <- fit_mnl(md1)
cat("Fixed-coefficient logit log-likelihood:", round(m0$loglik, 3), "\n")
m1 <- fit_mxl(md1, c(m0$coef, abs(m0$coef) / 2))
cat("Model 1 log-likelihood:", round(m1$loglik, 3), "\n")
b1 <- m1$coef
m2 <- fit_mxl(md2, c(b1[1:5], rep(0, 5), abs(b1[6:10]), abs(b1[6:10]) / 2))
cat("Model 2 log-likelihood:", round(m2$loglik, 3), "\n")
stopifnot(m1$loglik >= m0$loglik, m2$loglik >= m1$loglik,
          m1$hessian_positive_definite, m2$hessian_positive_definite)

stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))

# Shah et al. (2019) Table 3: published mean coefficients and significance.
paper3 <- data.frame(
  model = c(rep("Model 1", 5), rep("Model 2", 10)),
  term = c(base, base, inter),
  paper_estimate = c(0.0069, 0.0360, 0.0096, -0.0055, 8.220,
                     0.0035, 0.0299, 0.005, -0.0052, 8.461, 0.00758, 0.0168, 0.0105, -0.0011, 0.225),
  paper_sig = c("**", "***", "***", "***", "***", "", "***", "", "***", "***", "", "**", "*", "**", ""))

coef_table <- function(m, label) {
  se <- sqrt(diag(m$vcov))
  p <- 2 * pnorm(-abs(m$coef / se))
  data.frame(model = label, term = names(m$coef), estimate = unname(m$coef), std_err = unname(se),
             z = unname(m$coef / se), p_value = unname(p), sig = stars(unname(p)))
}
t3 <- rbind(coef_table(m1, "Model 1"), coef_table(m2, "Model 2"))
t3 <- merge(t3, paper3, by = c("model", "term"), all.x = TRUE, sort = FALSE)
t3 <- t3[order(t3$model, match(t3$term, c(names(m2$coef)))), ]
write.csv(t3, "outputs/table3_coefficients.csv", row.names = FALSE)

wtp <- function(m, num, den) {
  b <- m$coef
  a <- sum(b[num]); c <- sum(b[den])
  g <- setNames(numeric(length(b)), names(b))
  g[num] <- -1 / c
  g[den] <- g[den] + a / c^2
  c(est = -a / c, se = sqrt(drop(t(g) %*% m$vcov %*% g)))
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
           "optimizer_rounds_model1", "optimizer_rounds_model2", "max_scaled_gradient_model1",
           "max_scaled_gradient_model2", "hessian_positive_definite_model1", "hessian_positive_definite_model2"),
  value = c(md1$n_panels, md1$n_sets, m1$n_draws, m0$loglik, m1$loglik, m2$loglik, m1$rounds, m2$rounds,
            signif(m1$max_abs_scaled_gradient, 3), signif(m2$max_abs_scaled_gradient, 3),
            m1$hessian_positive_definite, m2$hessian_positive_definite))
write.csv(summ, "outputs/model_summary.csv", row.names = FALSE)

num <- vapply(t3, is.numeric, logical(1))
t3p <- t3; t3p[num] <- lapply(t3p[num], round, 4)
print(t3p, row.names = FALSE)
num <- vapply(t4, is.numeric, logical(1))
t4p <- t4; t4p[num] <- lapply(t4p[num], round, 3)
print(t4p, row.names = FALSE)
