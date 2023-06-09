## Algo:
# Remember that we do not need f here, but can be necessary if function value at optimum is needed.

NM <- function(f, f_prime, f_sec, dX0, dTol = 1e-9, n.max = 1000){
  dX <- dX0
  fx <- f(dX) # can be deleted
  fpx <- f_prime(dX)
  fsx <- f_sec(dX)
  n <- 0
  while ((abs(fpx) > dTol) && (n < n.max)) {
    dX <- dX - fpx/fsx
    fx <- f(dX) # can be deleted
    fpx <- f_prime(dX)
    fsx <- f_sec(dX)
    n <- n + 1
    cat("At iteration", n, "the value of x is:", dX, "\n")
  }
  if (n == n.max) {
    cat('newton failed to converge\n')
  } else {
    return(dX)
  }
}
