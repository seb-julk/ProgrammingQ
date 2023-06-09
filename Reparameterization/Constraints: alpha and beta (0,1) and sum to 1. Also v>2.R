# We start by defining the functions that allows us to invert the input and then reparameterize them
# We have the constraints in the example that needs to be adjusted accordingly:
#   0 < alpha_1 < 1
#   0 < alpha_2 < 1
#   0 < beta < 1
#   alpha_1 + alpha_2 + beta = 1
#   v > 2

# Defining two functions that takes the input and use them for reparameterization as we can rescale them with dUpper and dLow
LogConstraint_inv <- function(dX, dUpper = 1-1e-4, dLow = 1e-4){
    dOut <- log((dX - dLow)/(dUpper - dX))
    return(dOut)
}

LogConstraint <- function(dX, dUpper = 1-1e-4, dLow = 1e-4){
    dOut <- dLow + (dUpper - dLow)/(1 + exp(-dX))
    return(dOut)
}

# First we convert our input so we can use them for in our reparameterization by taking the invert
# OBS: note that in the code we do not define beta here. This simply follows that we define beta using our alphas hence
# we do not convert the initial guess.
ToTilde <- function(vPar){
    dUpper <- 1-1e-4
    dLow <- 1e-4
    
    dAlpha_1 <- vPar[1]
    dAlpha_2 <- vPar[2]
    dV  <- vPar[4] # as we must insert all our parameters then beta is vPar[3] but we do not use it here.
    
    dAlpha_1_tilde <- LogConstraint_inv(dAlpha_1, dUpper, dLow) # We define alpha1_tilde to be between 0,1
    dAlpha_2_tilde <- LogConstraint_inv(dAlpha_2, dUpper - dAlpha_1, dLow) # We define alpha2_tilde to be between 0,1-alpha1_tilde
        # This iteration can easily be extended to include more parameters and new boundaries as well.
        # But if the parameters are free (not be between values, then use "sum to 1..."
    dV_tilde  <- log(dV + 2)  # We define that dV must be positive and above 2
    
    vPar_tilde <- c(dAlpha_1_tilde, dAlpha_2_tilde, dV_tilde)
    return(vPar_tilde)
}

# Now we can use reparameterization for the converted input found above in vPar_tilde
# Note that we here use dAlpha_1 and dAlpha_2 to define dBeta which we ofcourse need and insure that it sum to 1
FromTilde <- function(vPar_tilde){
    dUpper <- 1-1e-4 # defining boundaries. These must match above
    dLow <- 1e-4
    
    dAlpha_1_tilde <- vPar_tilde[1]
    dAlpha_2_tilde <- vPar_tilde[2]
    dV_tilde  <- vPar_tilde[3]
    
    dAlpha_1 <- LogConstraint(dAlpha_1_tilde, dUpper, dLow) # now we use the LogConstraint which is the exponential reparamerization
    dAlpha_2 <- LogConstraint(dAlpha_2_tilde, dUpper - dAlpha_1, dLow) # We subtract agian. Remember that this should extended if we include more parameters
    dBeta <- 1 - dAlpha_1 - dAlpha_2
    dV  <- exp(dV_tilde) + 2 # insures that dV is above 2
    
    vPar <- c(dAlpha_1, dAlpha_2, dBeta, dV)
    return(vPar)
}

# Now we take the likelihood function which is already defined:
LLK_r <- function(theta_tilde, vData){
    # As we insert the parameters from ToTilde (already converted) then we must 
    theta <- FromTilde(theta_tilde)
    
    LLK(theta, vX = vData, f = dt, df = theta[4])
}


# 2.4 We try to optimize it:
theta = c(0.1, 0.1,0.8, 5) # we define the inital parameters provided in the problem
optimize <- optim(ToTilde(theta), LLK_r, vData = vData, method = "BFGS") # Note that the parameters inserted is inserted into ToTilde

FromTilde(optimize$par) # to get the reparameterized values we use the FromTilde

















