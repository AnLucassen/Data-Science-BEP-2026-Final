reticulate::use_python("C:/Users/20234345/AppData/Local/anaconda3/envs/BEP_DS_2026/python.exe",
                       required = TRUE)
library(lavaan)
library(subgroupsem)

# Modelling
# 1. Specify model in lavaan syntax
model <- "
  # visual =~ c(la01,la11)*x1 + c(NA, NA)*x1 + c(la02,la12)*x2 + c(la03,la13)*x3
  visual =~ NA*x1 + x2 + x3
  textual =~ NA*x4 + x5 + x6
  speed =~ NA*x7 + x8 + x9
  
  visual ~~ 1*visual
  textual ~~ 1*textual
  speed ~~ 1*speed
  
  visual + textual + speed ~ 0*1
  
  # group % c(gw0,gw1)*w
"

# 2. Choose Covariates
covariates <- c("sex", "school", "grade")

# 3./4. Run LRT-based subgroup discovery with DFS

# 3. Define interestingness measure
# qf <- "
# N1 := exp(gw1)
# diff1 := abs(la01 - la11)
# diff2 := abs(la02 - la12)
# diff3 := abs(la03 - la13)
# 
# subsem_qf := sqrt(N1) * (diff1 + diff2 + diff3)
# "


fit <- subsem_lrt(
  model = model,
  data = HolzingerSwineford1939,
  predictors = covariates
)

# fit <- subsem(
#   model = model,
#   data = HolzingerSwineford1939,
#   qf = qf,
#   predictors = covariates,
#   lavaan_options = list(group.w.free = TRUE))

summary(fit)

dir.create("results", showWarnings = FALSE)

res <- fit@summary_statistics
res$subgroup <- sapply(res$subgroup, as.character)
res$search_time_s <- round(as.numeric(fit@time_elapsed, units = "secs"), 2)
res$date <- format(Sys.time(), "%Y-%m-%d %H:%M")

write.csv(res, "results/kiefer.csv", row.names = FALSE)

# ---- Compare with the paper (Kiefer et al. 2024, Table 5, left side) ----
expected <- data.frame(
  subgroup = c("school=='Grant-White'", "school=='Pasteur'",
               "grade==7 AND school=='Grant-White'",
               "grade==7 AND school=='Grant-White' AND sex==2", "grade==7"),
  size_sg  = c(145, 156, 79, 43, 157),
  quality  = c(111.09483, 111.09483, 107.82037, 99.36034, 97.03249)
)
top5 <- head(res, 5)
same_values    <- isTRUE(all.equal(top5$quality, expected$quality, tolerance = 1e-6))
same_subgroups <- setequal(paste(top5$subgroup, top5$size_sg),
                           paste(expected$subgroup, expected$size_sg))

# ---- Run again: must give exactly the same numbers ----
fit2 <- subsem_lrt(model = model, data = HolzingerSwineford1939, predictors = covariates)
deterministic <- identical(fit@summary_statistics$quality, fit2@summary_statistics$quality)

cat("Same values as the paper:   ", same_values, "\n")
cat("Same subgroups as the paper:", same_subgroups, "\n")
cat("Same result when run again: ", deterministic, "\n")
