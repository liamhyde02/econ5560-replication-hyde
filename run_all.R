# Master script: runs the whole replication from raw data to final outputs.
#
# Usage, from the repo root:
#   Rscript run_all.R > logs/run_log.txt 2>&1
#
# Runs the scripts in code/ in order. Each one reads data/raw/ or an earlier
# output and writes to outputs/. The run stops at the first error.
# Needs R (4.1 or newer) with the mlogit, dfidx, haven and readxl packages.

steps <- c(
  "code/01_check_data.R",
  "code/02_table2_descriptives.R",
  "code/03_mixed_logit_tables3_4.R",
  "code/04_figure1_wtp.R"
)

options(width = 120)
dir.create("outputs", showWarnings = FALSE)
start <- Sys.time()
for (step in steps) {
  t0 <- Sys.time()
  cat("\n===== ", step, " =====\n", sep = "")
  source(step, local = new.env(), echo = FALSE)
  cat(sprintf("----- %s finished in %.1f s\n", step, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
cat(sprintf("\nAll steps finished. Total run time: %.1f s\n\n",
            as.numeric(difftime(Sys.time(), start, units = "secs"))))

print(sessionInfo())
