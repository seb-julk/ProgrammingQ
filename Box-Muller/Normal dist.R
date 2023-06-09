### Simulate standard bivariate normal random variables ###
#
# This function uses Box-Muller transformation
#
# Follows slide 38
###
# 0. Clear the environment
rm(list = ls())

# 1. Define the Box Muller function
BoxMuller <- function(size = 1) {
  
  # size: number of standard bivariate normal random variables
  # to simulate
  
  U <- runif(size)
  V <- runif(size)
  
  X <- sqrt(-2*log(U)) * cos(2*pi*V)
  Y <- sqrt(-2*log(U)) * sin(2*pi*V)

  return(c(X,Y))  
  
}


#2. A test of the function: simulate

set.seed(10086)
Z <- BoxMuller(size = 500)

#3. Plot the histogram

hist(Z, breaks = 25, freq = FALSE, 
     main = "Theoretical and simulated N(0,1) density", 
     col = "cornflowerblue", 
     xlim = c(-3.5, 3.5), ylim = c(0,0.45), 
     xlab = "x",
     cex.main = 0.8)

xPoint = seq(-3.5, max(Z)+0.5, 0.1)
lines(xPoint, dnorm(xPoint), col = "red")

legend("topright", legend = c("Simulated", "Theoretical"), lty = c(1, 1),
       lwd = c(5, 1), col = c("cornflowerblue", "red"))

axis(1, at = seq(-3.5, 3.5, by = 0.5))
