# We have the following constraints:
#     1 > alpha_A > 0
#     1 > alpha_B > 0
#     omega_A > 0
#     omega_B > 0
#     1 > beta_A > 0
#     1 > beta_B > 0
#     1 > rho > -1

# Defining two functions that takes the input and use them for reparameterization as we can rescale them with dUpper and dLow
Constraint_inv <- function(dX, dUpper = 1-1e-4, dLow = 1e-4){
    dOut <- log((dX - dLow)/(dUpper - dX))
    return(dOut)
}

Constraint <- function(dX, dUpper = 1-1e-4, dLow = 1e-4){
    dOut <- dLow + (dUpper - dLow)/(1 + exp(-dX))
    return(dOut)
}

# Inverting our initial guesses in order to use them in reparameterization
ToTilde <- function(vPar){
    dUpper <- 1-1e-4
    dLow <- 1e-4
    
    alpha_A_tilde <- Constraint_inv(vPar[1], dUpper, dLow)
    alpha_B_tilde <- Constraint_inv(vPar[2], dUpper, dLow)
    omega_A_tilde <- log(vPar[3])
    omega_B_tilde <- log(vPar[4])
    beta_A_tilde <-  Constraint_inv(vPar[5], dUpper, dLow)
    beta_B_tilde <- Constraint_inv(vPar[6], dUpper, dLow)
    rho_tilde <- Constraint_inv(vPar[7], dUpper, -1+1e-6) # changing lower limit 
    
    vPar_tilde <- c(alpha_A_tilde, alpha_B_tilde, omega_A_tilde, omega_B_tilde, beta_A_tilde, beta_B_tilde,rho_tilde)
    return(vPar_tilde)
}

# Reparameterization function:
FromTilde <- function(vPar_tilde){
    dUpper <- 1-1e-4 # defining boundaries. These must match above
    dLow <- 1e-4
    
    alpha_A <- Constraint(vPar_tilde[1], dUpper, dLow)
    alpha_B <- Constraint(vPar_tilde[2], dUpper, dLow)
    omega_A <- exp(vPar_tilde[3])
    omega_B <- exp(vPar_tilde[4])
    beta_A <-  Constraint(vPar_tilde[5], dUpper, dLow)
    beta_B <- Constraint(vPar_tilde[6], dUpper, dLow)
    rho <- Constraint(vPar_tilde[7], dUpper, -1+1e-4)
    
    vPar <- c(alpha_A, alpha_B, omega_A, omega_B, beta_A, beta_B,rho)
    return(vPar)
}

LogLLK_r <- function(vPar, mData){
    
    vPar <- FromTilde(vPar)
    
    alpha_A <- vPar[1]
    alpha_B <- vPar[2]
    omega_A <- vPar[3]
    omega_B <- vPar[4]
    beta_A <-  vPar[5]
    beta_B <- vPar[6]
    rho <- vPar[7]
    
    #REST OF LIKELIHOOD FUNCTION HERE:
    
 }

# We insert the initial parameters in the optim command by using our ToTilde function
optimizer <- optim(ToTilde(vPar), LogLLK_r, mData = mData, method = "BFGS")

# Only necessary if we need to reparameterize the output:
FromTilde(optimizer$par)



