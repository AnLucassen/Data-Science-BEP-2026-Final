

reticulate::use_python("C:/Users/20234345/AppData/Local/anaconda3/envs/BEP_DS_2026/python.exe",
                       required = TRUE)
library(lavaan)
library(subgroupsem)
cat("subgroupsem version:", as.character(packageVersion("subgroupsem")), "\n")

model <- "
  visual =~ NA*x1 + x2 + x3
  textual =~ NA*x4 + x5 + x6
  speed =~ NA*x7 + x8 + x9
  visual ~~ 1*visual
  textual ~~ 1*textual
  speed ~~ 1*speed
  visual + textual + speed ~ 0*1
"
covariates <- c("sex", "school", "grade")

#one DFS search with the log on
fit_log <- subsem_lrt(model = model, data = HolzingerSwineford1939, predictors = covariates,
                      subsem_options = list(algorithm = "DFS", keep_log = TRUE))
log <- fit_log@log
cnt <- fit_log@counters
print(unlist(cnt))
head(log, 10)

#in a beautiful table!
View(log)

#checking counting is correct
counts_add_up <- cnt$n_evaluated == cnt$n_below_30 + cnt$n_below_min_size +
  cnt$n_root + cnt$n_fit
one_row_per_description <- nrow(log) == cnt$n_evaluated
counts_add_up
#output: twice true so that worked!

#check that the log did not change anything, so top-10 must be the same
res_log <- fit_log@summary_statistics
res_log$subgroup <- sapply(res_log$subgroup, as.character)
original <- read.csv("results/kiefer.csv")

log_changes_nothing <-
  isTRUE(all.equal(res_log$quality, original$quality, tolerance = 1e-10)) &&
  identical(res_log$subgroup, original$subgroup)
log_changes_nothing

check <- data.frame(
  check = c("every description is counted exactly once",
            "the log has one row per description",
            "with log: same result as the original DFS"),
  ok = c(counts_add_up, one_row_per_description, log_changes_nothing)
)
print(check, row.names = FALSE)
one_row_per_description
#table says all true 3 times and last one true as well

#time divided from report and math and from google
fitted <- log$size >= 30 & log$depth > 0
failed <- fitted & log$quality == -1
search_s <- as.numeric(fit_log@time_elapsed, units = "secs")

time_split <- data.frame(
  part = c("whole search", "all fits", "successful fits", "failed fits",
           "everything else (generating candidates, too-small ones)"),
  seconds = round(c(search_s,
                    sum(log$seconds[fitted]),
                    sum(log$seconds[fitted & !failed]),
                    sum(log$seconds[failed]),
                    search_s - sum(log$seconds[fitted])), 2)
)
time_split$share_pct <- round(100 * time_split$seconds / search_s, 1)
print(time_split, row.names = FALSE)

cat("Fits:", sum(fitted), " of which failed:", sum(failed), "\n")

#which are slow
log[order(-log$seconds), c("description", "size", "quality", "seconds")][1:5, ]

#comparisons of the 4 algo and save 
work <- do.call(rbind, lapply(c("DFS", "Apriori", "BestFirst", "Beam"), function(a) {
  f <- subsem_lrt(model = model, data = HolzingerSwineford1939, predictors = covariates,
                  subsem_options = list(algorithm = a))
  data.frame(algorithm = a, f@counters,
             search_time_s = round(as.numeric(f@time_elapsed, units = "secs"), 2))
}))
print(work, row.names = FALSE)

write.csv(log, "results/candidate_log_dfs_kiefer.csv", row.names = FALSE)
write.csv(check, "results/candidate_log_check.csv", row.names = FALSE)
write.csv(time_split, "results/time_split_dfs_kiefer.csv", row.names = FALSE)
write.csv(work, "results/work_per_algorithm_kiefer.csv", row.names = FALSE)