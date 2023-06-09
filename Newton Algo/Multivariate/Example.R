### Multivariate Gauss-Newton Method ###
# OBS! Here f3 is defined as a list that includes function at [1], f_prime at [2] and hessian at [3]

newton <- function(f3, vX0, dTol = 1e-9, n.max = 100) {
  # Newton's method for optimisation, starting at x0
  # f3 is a function that given x returns the list
  # {f(x), grad f(x), Hessian f(x)}, for some f
  vX <- vX0
  f3.x <- f3(vX)
  n <- 0
  while ((max(abs(f3.x[[2]])) > dTol) & (n < n.max)) {
    vX <- vX - solve(f3.x[[3]], f3.x[[2]])
    f3.x <- f3(vX)
    cat("At iteration", n, "the coordinates of x are", vX, "\n")
    n <- n + 1
  }
  if (n == n.max) {
    cat('newton failed to converge\n')
  } else {
    return(vX)
  }
}


# Example of f3:
# stack all functions for convenience
f3 <- function(vBeta,vY,mX){
    flist <- list("loglik"=fLnL(vBeta,vY,mX),
                  "score"=fScore(vBeta,vY,mX),
                  "hessian"=fHessian(vBeta,vY,mX))
    return(flist)
  }
  
