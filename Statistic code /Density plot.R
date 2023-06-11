# Different density plots that are superimposed

# Gaussian dist:
hist(estimates$mu, breaks = 50, freq = FALSE)
lines(seq(0,12,by=0.01), dnorm(seq(0,12,by = 0.01), mean = mean(estimates$mu), sd = sd(estimates$mu)), type = "l", col = "red")
hist(estimates$sigma, breaks = 50, freq = FALSE)
lines(seq(0,12,by=0.01), dnorm(seq(0,12,by = 0.01), mean = mean(estimates$sigma), sd = sd(estimates$sigma)), type = "l", col = "red")


# Exponential dist:
h <- hist(X, freq = FALSE, col = "cornflowerblue", 
          main = "", 
          xlab = "x")
lines(seq(0,12,by=0.01), dexp(seq(0,12,by = 0.01), rate = 0.7), type = "l", col = "red")
