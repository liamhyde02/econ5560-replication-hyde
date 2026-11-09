# Step 1: check that the raw data are the files the team received, unchanged.
#
# Checks the MD5 hash of both raw files, the panel structure, and that the
# observed-choice spreadsheet matches the Dataverse file on all 19 shared
# columns. Writes outputs/data_checks.csv and stops if any check fails.

dta_path <- "data/raw/data_impact_community_based_mgt_on_MPAs.dta"
xlsx_path <- "data/raw/MPA_journal_data_with_choice_for_CalPolyFA2026.xlsx"
expected_md5 <- c(
  "data_impact_community_based_mgt_on_MPAs.dta" = "b79ce0221a0cddd7584fda902e1b10d8",
  "MPA_journal_data_with_choice_for_CalPolyFA2026.xlsx" = "af158b9f6f1bf9b4eb211361c5816f2c"
)

checks <- data.frame(check = character(), passed = logical(), detail = character())
check <- function(name, ok, detail) {
  checks[nrow(checks) + 1, ] <<- list(name, isTRUE(ok), detail)
  cat(sprintf("[%s] %s: %s\n", if (isTRUE(ok)) "PASS" else "FAIL", name, detail))
  if (!isTRUE(ok)) stop("Data check failed: ", name)
}

for (f in c(dta_path, xlsx_path)) {
  h <- unname(tools::md5sum(f))
  check(paste("md5", basename(f)), h == expected_md5[[basename(f)]], h)
}

d <- as.data.frame(haven::zap_labels(haven::read_dta(dta_path)))
x <- as.data.frame(readxl::read_excel(xlsx_path, sheet = "Data"))

check("dta rows", nrow(d) == 14832, paste(nrow(d), "rows (respondent x choice set x alternative)"))
check("dta columns", ncol(d) == 19, paste(ncol(d), "columns; no chosen-alternative variable"))
check("respondents", length(unique(d$record_id)) == 824,
      paste(length(unique(d$record_id)), "respondents (paper reports 827)"))
check("choice sets", length(unique(d$Cid)) == 4944, paste(length(unique(d$Cid)), "choice sets, 6 per respondent"))
check("3 alternatives per set", all(table(d$Cid) == 3), "two MPA options plus a status quo")
check("one status quo per set", all(tapply(d$ASC == 0, d$Cid, sum) == 1), "ASC = 0 once per set")

check("xlsx rows", nrow(x) == 14832, paste(nrow(x), "rows"))
check("one choice per set", all(tapply(x$Choice, x$Cid, sum) == 1), "exactly one Choice = 1 per Cid")
check("status quo is Alt 3", all(x$ASC[x$Alt == 3] == 0), "Alt 3 always has ASC = 0")

shared <- names(d)
m <- merge(d, x[, shared], by = "record_iidd", suffixes = c("_dta", "_xlsx"))
same <- all(vapply(setdiff(shared, "record_iidd"), function(v) {
  all(as.numeric(m[[paste0(v, "_dta")]]) == as.numeric(m[[paste0(v, "_xlsx")]]))
}, logical(1)))
check("xlsx matches dta", nrow(m) == 14832 && same,
      "all 19 shared columns identical after merging on record_iidd")

write.csv(checks, "outputs/data_checks.csv", row.names = FALSE)
cat("Wrote outputs/data_checks.csv\n")
