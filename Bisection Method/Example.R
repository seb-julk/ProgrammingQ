# Bisection algo, where function must be defined


bisection <- function(f, dX.l, dX.r, dTol = 1e-9, ...) {
  
  #check inputs
  if (dX.l >= dX.r) {
    cat("error: x.l >= x.r \n")
    return(NULL)
  }
  f.l <- f(dX.l, ...)
  f.r <- f(dX.r, ...)
  if (f.l == 0) {
    return(dX.l)
  } else if (f.r == 0) {
    return(dX.r)
  } else if (f.l*f.r > 0) {
    cat("error: f(x.l)*f(x.r) > 0 \n")
    return(NULL)
  }
  
  # successively refine x.l and x.r
  iter <- 0
  while ((dX.r - dX.l) > dTol) {
    dX.m <- (dX.l + dX.r)/2
    f.m <- f(dX.m, ...)
    if (f.m == 0) {
      return(dX.m)
    } else if (f.l*f.m < 0) {
      dX.r <- dX.m
      f.r <- f.m
    } else {
      dX.l <- dX.m
      f.l <- f.m
    }
    iter <- iter + 1
    cat("at iteration", iter, "the root lies between", dX.l, "and", dX.r, "\n")
  }
  
  # return approximate root
  return((dX.l + dX.r)/2)
}
