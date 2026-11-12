

?simulateData


library(lavaan)

# where the numbers below come from:
#parameterEstimates(sem(analysis_model, data = HolzingerSwineford1939))[, 1:4]

population.model <- "
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

#generate data 
set.seed(42)
myData <- lavSimulateData(population.model, sample_nobs = 1000L)

#population moments and sample moments 
fitted(sem(population.model))
round(cov(myData), 3)
round(colMeans(myData), 3)

#fit model 
myModel <- '
  visual =~ NA*x1 + x2 + x3
  textual =~ NA*x4 + x5 + x6
  speed =~ NA*x7 + x8 + x9
  visual ~~ 1*visual
  textual ~~ 1*textual
  speed ~~ 1*speed
  visual + textual + speed ~ 0*1
'
fit <- sem(myModel, data = myData)
summary(fit)

#added: covariates - subgroup are descrbied by these 
myData$C1  <- sample(c("a", "b", "c"), 1000, replace = TRUE)
myData$C2  <- sample(c("a", "b", "c"), 1000, replace = TRUE)
myData$Dc1 <- sample(c("a", "b", "c"), 1000, replace = TRUE)
myData$Dc2 <- sample(c("a", "b", "c"), 1000, replace = TRUE)
myData$Dn1 <- rnorm(1000)
myData$Dn2 <- rnorm(1000)

#added: planted subgroup 
#rows get new test scores from population model with halved visual loadings
planted <- myData$C1 == "a" & myData$C2 == "a"
sum(planted)
population.model.planted <- sub("visual  =~ 0.900*x1 + 0.498*x2 + 0.656*x3",
                                "visual  =~ 0.450*x1 + 0.249*x2 + 0.328*x3",
                                population.model, fixed = TRUE)
plantedData <- lavSimulateData(population.model.planted, sample_nobs = sum(planted))
myData[planted, paste0("x", 1:9)] <- plantedData[, paste0("x", 1:9)]

#save
dir.create("data/sim", recursive = TRUE, showWarnings = FALSE)
write.csv(myData, "data/sim/seed42.csv", row.names = FALSE)
str(myData)

getwd()
