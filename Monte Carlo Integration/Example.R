## Monte carlo integration
# Purpose: Measure the area of the unit circle
rm(list = ls())
cat("\014")

# MC integrate the area of a unit circle
iDraw <- 1000 # Number of draws

U <- runif(iDraw) # Draws for U
V <- runif(iDraw) # Draws for V

R <- U^2 + V^2 # Calculate radius squared

mean( (R < 1)/pi ) 

mean( R < 1 ) * 4  # Monte carlo integrate and multiply by 4 to obtain an approximation to pi

### Example from slides:
monte_carlo_integration <- function(func, n, a, b) {
    U <- runif(n, min = a, max = b)

    I <- (b - a) * mean(func(U))

    return(I)
}

objective <- function(x) {
   # some function f, where we want to estimate integration from a to b

   return(
       1 / (1 + x**2)
   )
}

monte_carlo_integration(
    func = objective,
    n = 1e6,
    a = -1,
    b = 1
)


# verify solution
integrate(f = objective, lower = -1, upper = 1)
