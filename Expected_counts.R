

#Counts
# expected number of candidates per design
#from simulated data 


count_rules <- function(s, d) {                 # first equation
  total <- 0
  for (k in 1:d) total <- total + sum(apply(combn(s, k), 2, prod))
  total
}
count_combinations <- function(s, d) {          # second equation
  sum(choose(sum(s), 1:d))
}

depth <- 3
rows <- list()

for (design in list.dirs("data/sim", full.names = FALSE, recursive = FALSE)) {
  dat <- read.csv(file.path("data/sim", design, "seed42.csv"))
  
  cat_cols <- grep("^Dc", names(dat), value = TRUE)
  num_cols <- grep("^Dn", names(dat), value = TRUE)
  
  # conditions per covariate: number of classes, or 5 intervals
  s <- c(sapply(cat_cols, function(col) length(unique(dat[[col]]))),
         rep(5, length(num_cols)))
  
  rows[[design]] <- data.frame(
    design     = design,
    depth      = depth,
    conditions = sum(s),
    best_first = count_rules(s, depth),
    apriori    = count_combinations(s, depth),
    dfs        = count_combinations(s, depth) + 1
  )
}

expected <- do.call(rbind, rows)
print(expected)
write.csv(expected, "results/expected_counts.csv", row.names = FALSE)