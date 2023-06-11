# We start by defining the functions that allows us to invert the input and then reparameterize them
# We have the constraints in the example that needs to be adjusted accordingly:
#   0 < alpha_1 < 1
#   0 < alpha_2 < 1
#   0 < beta < 1
#   alpha_1 + alpha_2 + beta < 1
# Note if boundaries changes for the parameters, this can easily be adjusted in the functions by changing 
# upper and lower limits


# Defining two functions that takes the input and use them for reparameterization as we can rescale them with dUpper and dLow
Constraint_inv <- function(dX, dUpper = 1-1e-4, dLow = 1e-4){
    dOut <- log((dX - dLow)/(dUpper - dX))
    return(dOut)
}

Constraint <- function(dX, dUpper = 1-1e-4, dLow = 1e-4){
    dOut <- dLow + (dUpper - dLow)/(1 + exp(-dX))
    return(dOut)
}

# First we convert our input so we can use them for in our reparameterization by taking the invert
# As the parameters are not summing to 1 anymore we define beta as well.
ToTilde <- function(vPar){
    dUpper <- 1-1e-4
    dLow <- 1e-4
    
    dAlpha_1 <- vPar[1]
    dAlpha_2 <- vPar[2]
    dBeta <- vPar[3]
    
    dAlpha_1_tilde <- Constraint_inv(dAlpha_1, dUpper, dLow) # We define alpha1_tilde to be between 0,1
    dAlpha_2_tilde <- Constraint_inv(dAlpha_2, dUpper - dAlpha_1, dLow) # We define alpha2_tilde to be between 0,1-alpha1_tilde
    # As the bounds remain the same we use dAlpha_1 in the boundary and NOT tilde!
    dBeta_tilde <- Constraint_inv(dAlpha_2, dUpper - dAlpha_1 - dAlpha_2, dLow) # follow the intuition above
    # This insures that the parameters sum < 1 and not exact 1
    
    vPar_tilde <- c(dAlpha_1_tilde, dAlpha_2_tilde, dBeta_tilde)
    return(vPar_tilde)
}

# Now we can use reparameterization for the converted input found above in vPar_tilde
# Note that we here use dAlpha_1 and dAlpha_2 to define dBeta which we ofcourse need and insure that it sum to 1
FromTilde <- function(vPar_tilde){
    dUpper <- 1-1e-4 # defining boundaries. These must match above
    dLow <- 1e-4
    
    dAlpha_1_tilde <- vPar_tilde[1]
    dAlpha_2_tilde <- vPar_tilde[2]
    dbeta_tilde  <- vPar_tilde[3]
    
    dAlpha_1 <- Constraint(dAlpha_1_tilde, dUpper, dLow) # now we use the LogConstraint which is the exponential reparamerization
    dAlpha_2 <- Constraint(dAlpha_2_tilde, dUpper - dAlpha_1, dLow) # We subtract agian. Remember that this should extended if we include more parameters
    dBeta_tilde <- Constraint_inv(dAlpha_2, dUpper - dAlpha_1 - dAlpha_2, dLow) # follow the intuition above
    
    vPar <- c(dAlpha_1, dAlpha_2, dBeta)
    return(vPar)
}

# Now we take the likelihood function which is already defined:
LLK_r <- function(theta_tilde, vData){

    # As we insert the parameters from ToTilde (already converted) then we must 
    theta <- FromTilde(theta_tilde)
    
    LLK(theta, vX = vData, f = dt, df = theta[4])
}


# 2.4 We try to optimize it:
theta = c(0.1, 0.1,0.8) # we define the inital parameters provided in the problem
optimize <- optim(ToTilde(theta), LLK_r, vData = vData, method = "BFGS") # Note that the parameters inserted is inserted into ToTilde

FromTilde(optimize$par) # to get the reparameterized values we use the FromTilde






