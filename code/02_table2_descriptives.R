# Step 2: reproduce Table 2 (respondent characteristics by survey version).
#
# Follows the team's R code (Zoey Miles's MeansTable.qmd and week7_outputs.R in
# the team repo): one row per respondent, gender code 2 = female and birth code
# 1 = Okinawa (both confirmed by the provider's data dictionary), income
# category codes 1-6 used literally, as the paper appears to do.
# Writes outputs/table2_descriptives.csv (with the published values beside ours)
# and outputs/table2_income_sensitivity.csv.

d <- as.data.frame(haven::zap_labels(haven::read_dta("data/raw/data_impact_community_based_mgt_on_MPAs.dta")))
dem <- c("record_id", "survey_type", "gender", "age", "annual_income", "birth_location", "children_HH")
p <- unique(d[dem])
stopifnot(!anyDuplicated(p$record_id))  # one demographic profile per respondent
p$age <- as.numeric(p$age)
p$children_HH <- as.numeric(p$children_HH)
p$female <- as.integer(p$gender == 2)
p$born_okinawa <- as.integer(p$birth_location == 1)
p$income_code <- as.numeric(p$annual_income)

vars <- c(female = "Female share", age = "Age (years; top answer is 56 and over)",
          income_code = "Income category code (1-6)", born_okinawa = "Born in Okinawa share",
          children_HH = "Children under 18 in household")
# Shah et al. (2019), Table 2: mean and SD, national then community. Birthplace SD not printed.
paper_mean <- list(female = c(0.50, 0.52), age = c(41.25, 41.95), income_code = c(3.63, 3.70),
                   born_okinawa = c(0.68, 0.70), children_HH = c(0.83, 0.75))
paper_sd <- list(female = c(0.50, 0.50), age = c(9.78, 9.36), income_code = c(1.43, 1.41),
                 born_okinawa = c(NA, NA), children_HH = c(1.13, 1.03))
groups <- c("National (top-down)", "Community (bottom-up)")

rows <- list()
for (g in 1:2) for (v in names(vars)) {
  xv <- p[p$survey_type == g, v]
  rows[[length(rows) + 1]] <- data.frame(
    group = groups[g], variable = vars[[v]], n = length(xv),
    mean = round(mean(xv), 4), sd_sample = round(sd(xv), 4),
    sd_population = round(sqrt(mean((xv - mean(xv))^2)), 4),
    paper_mean = paper_mean[[v]][g], paper_sd = paper_sd[[v]][g],
    mean_matches_2dp = round(mean(xv), 2) == paper_mean[[v]][g])
}
t2 <- do.call(rbind, rows)
write.csv(t2, "outputs/table2_descriptives.csv", row.names = FALSE)

inc <- do.call(rbind, lapply(1:2, function(g) {
  xv <- p$income_code[p$survey_type == g]
  data.frame(group = groups[g], codes_used = c("1-6 (as in paper)", "1-5 (code 6 = no answer dropped)"),
             n = c(length(xv), sum(xv != 6)), mean = round(c(mean(xv), mean(xv[xv != 6])), 4))
}))
write.csv(inc, "outputs/table2_income_sensitivity.csv", row.names = FALSE)

print(t2, row.names = FALSE)
cat(sprintf("Respondents: %d (%d national, %d community)\n", nrow(p),
            sum(p$survey_type == 1), sum(p$survey_type == 2)))
cat(sprintf("Means matching the paper to 2 decimals: %d of %d\n", sum(t2$mean_matches_2dp), nrow(t2)))
