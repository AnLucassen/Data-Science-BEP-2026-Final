

#memory 

one_run <- function(algorithm) {
  # a fresh session has nothing loaded: choose Python first, then the packages
  reticulate::use_python("C:/Users/20234345/AppData/Local/anaconda3/envs/BEP_DS_2026/python.exe",
                         required = TRUE)
  library(lavaan)
  library(subgroupsem)
  
  # memory of this R process (R + Python + lavaan together), in MB
  mem_mb <- function() {
    m <- ps::ps_memory_info()
    c(now = unname(m[["rss"]]) / 2^20, peak = unname(m[["peak_wset"]]) / 2^20)
  }
  
  model <- "
    visual =~ NA*x1 + x2 + x3
    textual =~ NA*x4 + x5 + x6
    speed =~ NA*x7 + x8 + x9
    visual ~~ 1*visual
    textual ~~ 1*textual
    speed ~~ 1*speed
    visual + textual + speed ~ 0*1
  "
  
  # warm-up (not measured): load pysubgroup and do one lavaan fit
  invisible(reticulate::import("pysubgroup"))
  d <- HolzingerSwineford1939
  d$g <- rep(1:2, length.out = nrow(d))
  invisible(lavaan::sem(model, data = d, group = "g", warn = FALSE))
  invisible(gc())
  
  # measure: before, search, after
  before <- mem_mb()
  fit <- subsem_lrt(model = model, data = HolzingerSwineford1939,
                    predictors = c("sex", "school", "grade"),
                    subsem_options = list(algorithm = algorithm))
  after <- mem_mb()
  
  data.frame(algorithm = algorithm,
             search_time_s = round(as.numeric(fit@time_elapsed, units = "secs"), 2),
             n_fit = fit@counters$n_fit,
             memory_before_mb = round(before[["now"]], 1),
             peak_after_mb = round(after[["peak"]], 1),
             extra_mb = round(after[["peak"]] - before[["now"]], 1))
}

memory <- do.call(rbind, lapply(c("DFS", "Apriori", "BestFirst", "Beam"), function(a) {
  cat("Running", a, "in a fresh R session ...\n")
  callr::r(one_run, args = list(algorithm = a))
}))
print(memory, row.names = FALSE)

write.csv(memory, "results/memory_kiefer.csv", row.names = FALSE)