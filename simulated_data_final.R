library(lavaan)
library(MASS)

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

create_sim_data <- function(
  population_model,
  settings,
  seed
  ) {
  
  # Set seed
  set.seed(seed)
  
  # Generate data
  myData <- lavSimulateData(population_model, sample_nobs = settings$sample_size)
  
  # Include planted covariates
  myData$C1  <- sample(c("a", "b", "c"), settings$sample_size, replace = TRUE)
  myData$C2  <- sample(c("a", "b", "c"), settings$sample_size, replace = TRUE)
  
  # Include distractors
  distractors <- settings$distractors
  n_distractors <- length(distractors)
  
    ## Build correlation matrix
  sigma_matrix <- matrix(settings$rho, nrow=n_distractors, ncol=n_distractors)
  diag(sigma_matrix) <- 1
  
  mv_data <- mvrnorm(n = settings$sample_size, mu = rep(0, n_distractors), Sigma = sigma_matrix)
  
  cat_count <- 0
  num_count <- 0
  
  for (i in seq_along(distractors)) {
    distractor <- distractors[[i]]
    col_data <- mv_data[, i] # the correlated continuous column
    
    if (distractor$type == "cat") {
      cat_count <- cat_count + 1
      
      ## Need to determine cut points to create equally likely categorical values
      quantiles <- quantile(
        col_data, 
        probs = seq(
          0, 
          1, 
          length.out = distractor$n_classes + 1
          )
        )
      
      quantiles[1] <- -Inf
      quantiles[length(quantiles)] <- Inf
      
      ## Assign categorical distractor value, based on equal quantiles, 
      ## which are labeled by letters of the alphabet.
      
      myData[[paste0("Dc", cat_count)]] <- 
        cut(
          col_data,
          breaks = quantiles,
          labels = head(letters, distractor$n_classes)
        )
        
    } else {
      num_count <- num_count + 1
      
      ## Assign numerical distractor value, normally distributed in mv_data
      myData[[paste0("Dn", num_count)]] <- col_data
    }
  }
  
  # # Define planted subgroup, and adjust the factor loading "visual" by half
  # planted <- myData$C1 == "a" & myData$C2 == "a"
  
  # population_model.planted <- sub(
  #   "visual  =~ 0.900*x1 + 0.498*x2 + 0.656*x3",
  #   "visual  =~ 0.450*x1 + 0.249*x2 + 0.328*x3",
  #   population_model, fixed = TRUE
  # )
  
  # # Generate adjusted values for planted subgroup & update myData
  # plantedData <- lavSimulateData(population_model.planted, sample_nobs = sum(planted))
  # myData[planted, paste0("x", 1:9)] <- plantedData[, paste0("x", 1:9)]
  
  return(myData)
}

datasets <- list(
  # 1. BASE DESIGN
  list(
    name = "base_design",
    sample_size = 1000L,
    rho = 0,
    distractors = list(
      list(type = "cat", n_classes = 3),
      list(type = "cat", n_classes = 3),
      list(type = "num"),
      list(type = "num")
    )
  ),
  
  # 2. SMALL SAMPLE SIZE
  list(
    name = "small_sample_size",
    sample_size = 500L,
    rho = 0,
    distractors = list(
      list(type = "cat", n_classes = 3),
      list(type = "cat", n_classes = 3),
      list(type = "num"),
      list(type = "num")
    )
  ),
  
  # 3. LARGE SAMPLE SIZE
  list(
    name = "large_sample_size",
    sample_size = 5000L,
    rho = 0,
    distractors = list(
      list(type = "cat", n_classes = 3),
      list(type = "cat", n_classes = 3),
      list(type = "num"),
      list(type = "num")
    )
  ),
  
  # 4. "FEW" DISTRACTORS
  list(
    name = "few_distractors",
    sample_size = 1000L,
    rho = 0,
    distractors = list(
      list(type = "cat", n_classes = 3),
      list(type = "num")
    )
  ),
  
  # 5. "MANY" DISTRACTORS
  list(
    name = "many_distractors",
    sample_size = 1000L,
    rho = 0,
    distractors = list(
      list(type = "cat", n_classes = 3), list(type = "cat", n_classes = 3),
      list(type = "cat", n_classes = 3), list(type = "cat", n_classes = 3),
      list(type = "num"), list(type = "num"),
      list(type = "num"), list(type = "num")
    )
  ),
  
  # 6. ONLY CATEGORICAL DISTRACTORS
  list(
    name = "only_categorical",
    sample_size = 1000L,
    rho = 0,
    distractors = list(
      list(type = "cat", n_classes = 3), list(type = "cat", n_classes = 3),
      list(type = "cat", n_classes = 3), list(type = "cat", n_classes = 3)
    )
  ),
  
  # 7. ONLY NUMERIC DISTRACTORS
  list(
    name = "only_numerical",
    sample_size = 1000L,
    rho = 0,
    distractors = list(
      list(type = "num"), list(type = "num"),
      list(type = "num"), list(type = "num")
    )
  ),
  
  # 8. VALUES PER CAT: 2
  list(
    name = "2_values_per_category",
    sample_size = 1000L,
    rho = 0,
    distractors = list(
      list(type = "cat", n_classes = 2),
      list(type = "cat", n_classes = 2),
      list(type = "num"),
      list(type = "num")
    )
  ),
  
  # 9. VALUES PER CAT: 6
  list(
    name = "6_values_per_category",
    sample_size = 1000L,
    rho = 0,
    distractors = list(
      list(type = "cat", n_classes = 6),
      list(type = "cat", n_classes = 6),
      list(type = "num"),
      list(type = "num")
    )
  ),
  
  # 10. CORRELATION: 0.3
  list(
    name = "correlation_moderate",
    sample_size = 1000L,
    rho = 0.3,
    distractors = list(
      list(type = "cat", n_classes = 3),
      list(type = "cat", n_classes = 3),
      list(type = "num"),
      list(type = "num")
    )
  ),
  
  # 11. CORRELATION: 0.7
  list(
    name = "correlation_high",
    sample_size = 1000L,
    rho = 0.7,
    distractors = list(
      list(type = "cat", n_classes = 3),
      list(type = "cat", n_classes = 3),
      list(type = "num"),
      list(type = "num")
    )
  )
)

for (seed in c(42, 43, 44)) {

  for (ith_dataset in seq_along(datasets)) {
    
    dataset_settings <- datasets[[ith_dataset]]
    
    simData <- create_sim_data(
      population_model = population.model,
      seed = seed,
      settings = dataset_settings
    )
    
    cat(paste0("Created dataset ", dataset_settings$name, " seed ", seed, "!\n"))
    
    dir.create(paste0("data/sim/", dataset_settings$name), recursive = TRUE, showWarnings = FALSE)
    write.csv(simData, paste0("data/sim/", dataset_settings$name, "/seed", seed, ".csv"), row.names = FALSE)
  }
}
