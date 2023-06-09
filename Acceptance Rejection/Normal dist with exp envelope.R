# 0. Clear the environment
rm(list = ls())

# Define a function by using exponential envilope.
Normal.Simulate <- function(size = 1) {
  
  Z <- rep(NA, size)
  
  for (i in 1:size) {
    
    ContinueLoop <- TRUE
    
    while (ContinueLoop) {
      V <- runif(1)
      Y <- -log(V)
      U <- runif(1)
      
      if (U <= exp(-0.5 * (Y-1)^2)) {
        S <- sample(c(1,-1), size = 1, prob = c(0.5, 0.5))
        Z[i] <- S*Y
        ContinueLoop <- FALSE
      }
    }
  }
  
  return(Z)
  
}

# A test: simulate and plot
X <- Normal.Simulate(1000)
hist(X, breaks = 25, freq = FALSE)
