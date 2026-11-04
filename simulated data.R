

?simulateData


library(lavaan)

seed <- 42
N <- 1000

analysis_model <- "
  visual =~ NA*x1 + x2 + x3
  textual =~ NA*x4 + x5 + x6
  speed =~ NA*x7 + x8 + x9
  visual ~~ 1*visual
  textual ~~ 1*textual
  speed ~~ 1*speed
  visual + textual + speed ~ 0*1
"
# where the numbers below come from:
parameterEstimates(sem(analysis_model, data = HolzingerSwineford1939))[, 1:4]

population_model <- "
  # factor loadings
  visual  =~ 0.900*x1 + 0.498*x2 + 0.656*x3
  textual =~ 0.990*x4 + 1.102*x5 + 0.917*x6
  speed   =~ 0.619*x7 + 0.731*x8 + 0.670*x9

  # factor variances, factor means and factor covariances
  visual ~~ 1*visual
  textual ~~ 1*textual
  speed ~~ 1*speed
  visual + textual + speed ~ 0*1
  visual ~~ 0.459*textual
  visual ~~ 0.471*speed
  textual ~~ 0.283*speed

  # residual variances
  x1 ~~ 0.549*x1
  x2 ~~ 1.134*x2
  x3 ~~ 0.844*x3
  x4 ~~ 0.371*x4
  x5 ~~ 0.446*x5
  x6 ~~ 0.356*x6
  x7 ~~ 0.799*x7
  x8 ~~ 0.488*x8
  x9 ~~ 0.566*x9

  # intercepts
  x1 ~ 4.936*1
  x2 ~ 6.088*1
  x3 ~ 2.250*1
  x4 ~ 3.061*1
  x5 ~ 4.341*1
  x6 ~ 2.186*1
  x7 ~ 4.186*1
  x8 ~ 5.527*1
  x9 ~ 5.374*1
"

population_model_planted <- sub(
  "visual  =~ 0.900*x1 + 0.498*x2 + 0.656*x3",
  "visual  =~ 0.450*x1 + 0.249*x2 + 0.328*x3",
  population_model, fixed = TRUE)
cat(population_model_planted)

set.seed(seed)
covariates <- data.frame(
  C1  = sample(c("a", "b", "c"), N, replace = TRUE),
  C2  = sample(c("a", "b", "c"), N, replace = TRUE),
  Dc1 = sample(c("a", "b", "c"), N, replace = TRUE),
  Dc2 = sample(c("a", "b", "c"), N, replace = TRUE),
  Dn1 = rnorm(N),
  Dn2 = rnorm(N)
)
head(covariates)
planted <- covariates$C1 == "a" & covariates$C2 == "a"
sum(planted)


data_rest    <- simulateData(population_model,         sample.nobs = sum(!planted), seed = seed + 1)
data_planted <- simulateData(population_model_planted, sample.nobs = sum(planted),  seed = seed + 2)
