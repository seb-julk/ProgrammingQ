### Newton-Raphson 
# F and f_prime must be redefined here and then NR can be used for the proper inputs
# When finding the guess it often helps to plot the function (see example)
#Example

f <- function(dR, dA, dP, iN) {
  dOut = dA/dP - (dR*(1.0 + dR)^iN)/((1.0 + dR)^iN - 1)
  return(dOut)
}
plot(vR, f(vR, dA = 10, dP = 100, iN = 20), type = "l") # plot example
abline(h = 0, col = "red")

f_prime <- function(dX, dA, dP, iN) {
  
  dNum1 = ((1.0 + dX)^iN + iN * dX * (1.0 + dX)^(iN - 1)) *
    ((1.0 + dX)^iN - 1)
  dNum2 = iN * dX * (1.0 + dX)^(2 * iN - 1)
  dDen  = ((1.0 + dX)^iN - 1)^2
  
  dOut = -(dNum1 - dNum2)/dDen
  return(dOut)
}

# Newton-Raphson function
NR <- function(f, f_prime, dX0, dTol = 1e-9, max.iter = 1000, ...)  {
  dX <- dX0
  fx <- f(dX, ...) 
  iter <- 0
  while((abs(fx)) > dTol && (iter < max.iter)) {
    dX <- dX - f(dX, ...)/f_prime(dX, ...)
    fx <- f(dX, ...)
    iter <- iter + 1
    cat("At iteration", iter, "value of x is:", dX, "\n")
  }
  if (abs(fx) > dTol) {
    cat("Algorithm failed to converge\n")
    return(NULL)
  } else {
    cat("Algorithm converged\n")
    return(dX)
  }
}
