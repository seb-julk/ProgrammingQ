  # We start by finding the benchmark results using glm commando
  # simulation
  set.seed(123)
  p <- 10
  n <- 2000
  Beta <- numeric(p)+ 1
  
  X <- matrix(rnorm(n*p),n,p) 
  Ys <- X%*%Beta + rlogis(n,0,1)
  Y <- Ys > 0
  
  Logit2 <- glm(Y ~  X,family=binomial)
  Logit2$coefficients
  
# Optimization:
# Problem set 5
# We definde the functions
Y_star <- function(mX,beta,eps){
    return(mX%*%beta+eps)
}

Pz <- function(z){
    return(1/(1+exp(-z)))
}

fLnL <- function(mX, beta, vY){
    vStorage <- rep(NA, length(vY))
    for (i in 1:length(vY)){
        pz <- Pz(mX%*%beta)
        vStorage[i] <- vY[i]*log(pz[i]) + (1-vY[i])*log(1-pz[i])
    }
    return(mean(vStorage))
}
fLnL(mX, beta, vY)

score <- function(mX, beta, vY){
    pz<- Pz(mX%*%beta)
    return(t(mX)%*%(pz - vY) / length(vY))
}
score(mX, beta, vY)

fheissan <- function(mX, beta, vY){
    pz<- as.numeric(Pz(mX%*%beta))
    s <- diag(pz*(1-pz))
    return(t(mX)%*%s%*%mX / length(vY))
}
fheissan(mX, beta, vY)

f3 <- function(mX, beta, vY){
    flist <- list("loglik"=fLnL(mX, beta, vY),
                  "score"=score(mX, beta, vY),
                  "hessian"=hessian(mX, beta, vY))
    return(flist)
}
f3

newton <- function(f3, vY, cons = TRUE, beta, dTol = 1e-16, n.max = 200) {
    if (cons == TRUE){
        vAdd <- rep(1, length(vY))
        mX <- cbind(vAdd, mX)
        beta_new <- c(0, beta)
        beta <- beta_new
    }
    beta0 <- rep(0, length(beta))
    
    n <- 0
    while ((max(abs(score(mX, beta, vY))) > dTol) & (n < n.max)) {
        beta <- beta - solve(fheissan(mX, beta, vY), score(mX, beta, vY))
        n <- n + 1
        print(n)
    }
    if (n == n.max) {
        cat('newton failed to converge\n')
    } else {
        return(list("Est beta" = beta,
                    "LLK" = mean(fLnL(mX,beta,vY)),
                    "Score" = score(mX,beta,vY),
                    "Hessian" = fheissan(mX,beta,vY),
                    "lik0" = mean(fLnL(mX, beta0, vY)),
                    "prob" = Pz(as.numeric(Pz(mX%*%beta))),
                    "Iter" = n))
        
    }
}
y <- newton(f3, vY = vY, beta = rep(0,10))
y$`Est beta`
