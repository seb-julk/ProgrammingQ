# We use the following functions in the barrier function and the penalty function

 # Prelim - Code from last time ---- 
 #golden section 
 gsection <- function(f, dX.l, dX.r, dX.m, dTol = 1e-9, ...) {
   
   # golden ratio plus one
   dGR1 <- 1 + (1 + sqrt(5))/2
   
   # successively refine x.l, x.r, and x.m
   f.l <- f(dX.l, ...)
   f.r <- f(dX.r, ...)
   f.m <- f(dX.m, ...)
   while ((dX.r - dX.l) > dTol) { 
     if ((dX.r - dX.m) > (dX.m - dX.l)) { # if the right segment is wider than the left 
       dY <- dX.m + (dX.r - dX.m)/dGR1 # put Y into the right segment according to the golden ratio
       f.y <- f(dY, ...)
       if (f.y >= f.m) {
         dX.l <- dX.m
         f.l <- f.m
         dX.m <- dY
         f.m <- f.y
       } else {
         dX.r <- dY
         f.r <- f.y
       }
     } else { #if the left segment is wider than the right
       dY <- dX.m - (dX.m - dX.l)/dGR1 # put Y into the left segment according to the golden ratio
       f.y <- f(dY, ...)
       if (f.y >= f.m) {
         dX.r <- dX.m
         f.r <- f.m
         dX.m <- dY
         f.m <- f.y
       } else {
         dX.l <- dY
         f.l <- f.y
       }
     }
   }
   return(dX.m)
 }
 
 #line search
 line.search <- function(f, vX, vG, dTol = 1e-9, dA.max = 2^5, ...) {
   # f is a real function that takes a vector of length d
   # x and y are vectors of length d
   # line.search uses gsection to find a >= 0 such that
   # g(a) = f(x + a*y) has a local maximum at a,
   # within a tolerance of tol
   # if no local max is found then we use 0 or a.max for a
   # the value returned is x + a*y
   if (sum(abs(vG)) == 0){
     return(vX) # +0*vG
   } # g(a) constant
   g <- function(dA, ...){
     return(f(vX + dA*vG, ...)) 
   }
   # find a triple a.l < a.m < a.r such that
   # g(a.l) <= g(a.m) and g(a.m) >= g(a.r)
   
   # choose a.l
   dA.l <- 0
   g.l <- g(dA.l, ...)
   # find a.m
   dA.m <- 1
   g.m <- g(dA.m, ...)
   while ((g.m < g.l) & (dA.m > dTol)) {
     dA.m <- dA.m/2
     g.m <- g(dA.m, ...)
   }
   # if a suitable a.m was not found then use 0 for a, so just return vX as the next step
   if ((dA.m <= dTol) & (g.m < g.l)){
     return(vX)
   } 
   # find a.r
   dA.r <- 2*dA.m
   g.r <- g(dA.r, ...)
   while ((g.m < g.r) & (dA.r < dA.max)) {
     dA.m <- dA.r
     g.m <- g.r
     dA.r <- 2*dA.m
     g.r <- g(dA.r, ...)
   }
   # if a suitable a.r was not found then use a.max for a
   if ((dA.r >= dA.max) & (g.m < g.r)){
     return(vX + dA.max*vG)
   } 
   # apply golden-section algorithm to g to find a
   dA <- gsection(g, dA.l, dA.r, dA.m, ...)
   return(vX + dA*vG)
 }
 
 #ascent function
 ascent <- function(f, grad.f, vX0, dTol = 1e-9, n.max = 100, verbose = TRUE, ...) {
   vX.old <- vX0
   vG0<-grad.f(vX0, ...)
   vX <- line.search(f, vX0, vG0, ...)
   n <- 1
   while ((f(vX, ...) - f(vX.old, ...) > dTol) & (n < n.max)) {
     vX.old <- vX
     vG <- grad.f(vX, ...)
     vX <- line.search(f, vX, vG, ...)
     if(verbose){
       cat("at iteration", n, "the coordinates of x are", vX, "\n")
     }
     n <- n + 1
   }
   return(vX)
 }
