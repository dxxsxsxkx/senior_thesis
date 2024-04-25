# utility functions used in careerEffect paper. 

expWeibull <- function(model_weibull, data_new){
  # Args: 
  #   model_weibull: a survreg object
  #   data_new: a data frame
  # Returns:
  #   a data frame
  data_stats <- data.frame(
    status = NA,
    mean = NA,
    mean_boot = NA, 
    se = NA, 
    ci_lower = NA, 
    ci_upper = NA
  )
  
  # iterate through the dataframe
  for (i in 1:nrow(data_new)) {
    # status
    data_stats[i, 1] <- colnames(data_new)[data_new[i,] == 1] %>% 
      # transform to one string, combining elements
      paste(collapse = "*")
    # parameters of the distribution
    kCoef <- model_weibull$coefficients
    kAlpha <- 1 / model_weibull$scale
    kLambda <- 1 / exp(sum(kCoef * data_new[i,]))
    
    # expected value
    data_stats[i, 2] <- gamma(1 + (1 / kAlpha)) / kLambda
    
    # stats
    data_stats[i, 3:6] <- bootWeibull(
      alpha = kAlpha, 
      lambda = kLambda, 
      num_sample = 1000, 
      num_iter = 1000
    )
  }
  
  return(data_stats)
}

bootWeibull <- function(alpha, lambda, num_sample, num_iter){
  # Args: 
  #   alpha: a numeric
  #   lambda: a numeric
  #   num_iter: a numeric
  # Returns:
  #   a numeric
  
  # parameters of the distribution
  kAlpha <- alpha
  kLambda <- lambda
  
  #
  kSample <- rweibull(
    n = num_sample, 
    shape = kAlpha, 
    scale = 1/kLambda
  )
  
  # 
  data.iter <- matrix(0, nrow = num_iter, ncol = num_sample)
  for (i in 1:num_iter) {
    # resample 1000 times
    data.iter[i, ] <- sample(kSample, size = num_sample, replace = TRUE)
  } 
  kMeans <- apply(data.iter, 1, mean)
  
  kMean <- mean(kMeans)
  kSE <- sd(kMeans)
  kCILower <- kMean - qnorm(1 - (0.05 / 2)) * kSE
  kCIUpper <- kMean + qnorm(1 - (0.05 / 2)) * kSE
  
  return(
    data.frame(
      mean = kMean, 
      se = kSE, 
      ci_lower = kCILower, 
      ci_upper = kCIUpper
    )
  )
}
