# Code is following practice exam 1
# Here we have the following constraints:
#     omega > 0
#     sum(alpha_p) = 1
#     mu is unconstraint
# If it must sum to 5 e.g. then we multiply with 5 in the respective line (here 22) in FromTilde 

# NOTE: we do not use ToTilde here as we insert the inverted parameters below.
# First we must convert the initial parameters. Note: as alpha must sum to 1 then the invers will be same in this case
mu <- 0
init <- mean(vData^2)
omega <- log(init)
alpha_1 <- 0.7
alpha_2 <- 0.3
vPar <- c(mu, omega, 0.7, 0.3)

# the reparameterization function:
FromTilde <- function(vPar){

    dMu <- vPar[1] # unconstraint
    dOmega <- exp(vPar[2]) # must be greater than zero
    vAlpha <- vPar[3:length(vPar)] / sum(vPar[3:length(vPar)]) # ensures that they sum to 1
    
    vPar <- c(dMu, dOmega, vAlpha)
    return(vPar)
}

# When we then write our likelihood function we must insert our vPar with initial parameters
# Inside the function we are then using the function FromTilde to convert the parameters.
# NOTE: remember to use the FromTilde again if we must provide the reparameterized values
