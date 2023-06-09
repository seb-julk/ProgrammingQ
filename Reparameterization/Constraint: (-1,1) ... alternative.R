# Alternative solution where we use the sigmoid function for (-1,1)
# And we do note use FromTilde here. 
# Should only be used if (-1,1) original does not work



ToTilde <- function(dX, dUpper = 1-1e-4, dLow = 1e-4){
    dOut <- log((dX - dLow)/(dUpper - dX))
    return(dOut)
}

LogLLKl <- function(mPar, mData){
    
    alpha_A <- exp(mPar[1])/(1+exp(mPar[1]))
    alpha_B <- exp(mPar[5])/(1+exp(mPar[5]))
    omega_A <- exp(mPar[2])
    omega_B <- exp(mPar[6])
    beta_A <-  exp(mPar[3])/(1+exp(mPar[3]))
    beta_B <- exp(mPar[7])/(1+exp(mPar[7]))
    rho <- (2*exp(mPar[4])) / (1 + exp(mPar[4])) - 1
    
    
    sigma2_A <- rep(NA, length(mData[,1]))
    sigma2_B <- rep(NA, length(mData[,1]))
    
    vY <- rep(NA, length(mData[,1])-2)
    r_t_A <- rep(NA, length(mData[,1])-1)
    r_t_B <- rep(NA, length(mData[,2])-1)
    
    sigma2_A[1] <- omega_A / (1-alpha_A-beta_A)
    sigma2_B[1] <- omega_B / (1-alpha_B-beta_B)
    
    N <- length(mData[,1]) -1
    
    for (i in 2:N){
        sigma2_A[i] <- omega_A  + alpha_A * mData[i-1,1]^2 + beta_A * sigma2_A[i-1]
        sigma2_B[i] <- omega_B  + alpha_B * mData[i-1,2]^2 + beta_B * sigma2_B[i-1]
        
        r_t_A <- mData[i,1]
        r_t_B <- mData[i,2]
        r_t <- c(r_t_A, r_t_B)
        
        Sigma <- matrix(c(sigma2_A[i], rho*sqrt(sigma2_A[i])*sqrt(sigma2_B[i]),
                          rho*sqrt(sigma2_A[i])*sqrt(sigma2_B[i]), sigma2_B[i]), nrow = 2, byrow = TRUE)
        inv_Sigma <- solve(Sigma)
        
        vY[i-1] <- - 0.5*log(det(Sigma)) - 1/2 * t(r_t) %*% inv_Sigma %*% r_t
        
    }
    return(-mean(vY))
    
    #}
    
}
mPar_log <- matrix(c(log(alpha_A/(1-alpha_A)), log(alpha_A/(1-alpha_B)),
                             log(omega_A), log(omega_B),
                             log(beta_A/(1-beta_A)), log(beta_B/(1-beta_B)),
                             ToTilde(rho, -1, 1), ToTilde(rho, -1, 1)), ncol = 2, byrow = TRUE)
vPar_log <- matrix(mPar_log)
    
    
LogLLKl(mPar_log, mData)

optimize <- optim(mPar_log, LogLLKl, mData = mData, method = "BFGS")
