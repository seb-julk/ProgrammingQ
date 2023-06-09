### Auxiliary functions ###

# Setting up the probability distribution for the Gamma distribution
gamma.pdf <- function(x, k, theta) {
    return(theta^k * x^(k - 1) * exp(-theta * x))
}

# Setting up the probability distributino for the Exponential function
exponential.pdf <- function(x, lambda) {
    return(lambda * exp(-lambda * x))
}

# The following function simulates by the Inversion Method the exponential distribution. 
Exponential.Simulate <- function(lambda, size = 1) {
    V <- runif(size)
    return(-1 / lambda * log(V))
}


### Simulate random variable having Gamma distribution ###
#
# Here we use vectorization so as to accelerate and the
# the function can simulate many i.i.d. random variables
# at a time
#
###


Gamma.Simulate <- function(k, theta, size = 1) {
    lambda <- theta / k
    c <- k^k * exp(-k + 1) / gamma(k)

    U <- rep(NA, size)
    Y <- rep(NA, size)
    X <- rep(NA, size)
    Unaccepted <- rep(TRUE, size)

    while (any(Unaccepted)) {
        UnacceptedCount <- sum(Unaccepted)

        U <- runif(UnacceptedCount)
        Y <- Exponential.Simulate(lambda, UnacceptedCount)

        Accepted_ThisTime <- Unaccepted[Unaccepted] &
            (U <= (gamma.pdf(Y, k, theta) / exponential.pdf(Y, lambda) / c)) # burde det ikke være gange c?

        X[Unaccepted][Accepted_ThisTime] <- Y[Accepted_ThisTime]
        Unaccepted[Unaccepted] <- !Accepted_ThisTime
    }

    return(X)
}

Gamma.Simulate(k, theta, size = 1)
