

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

#function that runs 4 algo instead writing all of them separately, from the test file
run <- function(algorithm) {
  cat("\nRunning", algorithm, "...\n")
  fit <- subsem_lrt(model = model, data = HolzingerSwineford1939, predictors = covariates,
                    subsem_options = list(algorithm = algorithm))
  res <- fit@summary_statistics
  res$subgroup <- sapply(res$subgroup, as.character)
  res$algorithm <- algorithm
  res$search_time_s <- round(as.numeric(fit@time_elapsed, units = "secs"), 2)
  res
}

algorithms <- c("DFS", "Apriori", "BestFirst", "Beam")
results <- lapply(algorithms, run)
names(results) <- algorithms

#result of one algo
results$Apriori[, c("quality", "subgroup", "size_sg")]

#is this DFS still the same as the original the kiefer_test
original <- read.csv("results/kiefer.csv")
dfs_same_as_original <-
  isTRUE(all.equal(results$DFS$quality, original$quality, tolerance = 1e-10)) &&
  identical(results$DFS$subgroup, original$subgroup) &&
  identical(as.numeric(results$DFS$size_sg), as.numeric(original$size_sg))
dfs_same_as_original
#output: says yes!!

#find the same 10 subgroups but then with apriori and best-first
same_top10 <- function(a, b) {
  isTRUE(all.equal(sort(a$quality), sort(b$quality), tolerance = 1e-10)) &&
    setequal(paste(a$subgroup, a$size_sg), paste(b$subgroup, b$size_sg))
}

check <- data.frame(
  check = c("my DFS == original DFS (same order)",
            "Apriori == DFS (same top-10)",
            "BestFirst == DFS (same top-10)",
            "Beam == DFS (information only, may be FALSE)"),
  ok = c(dfs_same_as_original,
         same_top10(results$Apriori, results$DFS),
         same_top10(results$BestFirst, results$DFS),
         same_top10(results$Beam, results$DFS))
)
print(check, row.names = FALSE)
#output: for all true this means that beam search and apriori and best first finds the same top-10

times <- sapply(results, function(r) r$search_time_s[1])
print(times)

write.csv(do.call(rbind, results), "results/four_algo_kiefer.csv", row.names = FALSE)
write.csv(check, "results/algo_check.csv", row.names = FALSE)

