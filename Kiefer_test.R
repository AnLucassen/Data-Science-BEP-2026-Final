# Dependencies
# install.packages("devtools")
# devtools::install_github("langenberg/subgroupsem")

# reticulate::install_python()
# reticulate::py_install("pysubgroup==0.7.8", envname = "r-reticulate")
# reticulate::use_virtualenv("r-reticulate", required = TRUE)

library(subgroupsem)
library(devtools)
library(lavaan)

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
