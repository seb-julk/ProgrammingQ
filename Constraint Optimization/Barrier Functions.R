 # 10.2 Barrier functions - (interior-points) ----
 # OBS see bottom where we define to an ascent function!
 #barrier function
 b<-function(vX, va, vb){
   gX<-c(vX-vb, va-vX)
   if(all(gX<=0)){
     return(-sum(log(-gX)))
   }else{
     return(Inf)
   }
 }
 #new objective with a barrier
 f_b<-function(vX, dGamma, ...){
   return(f(vX)-dGamma*b(vX, ...)) #subtract the penalty since we're maximizing
 }
 
 grad.f_b<-function(vX, dGamma, ...){
   return(grad(func=f_p, x=vX, dGamma=dGamma, ...)) # using numerical derivatives here because I'm lazy
 }
 
 # We then wanna choose starting values:
 #gamma_h=1
 vX_star<-ascent(f_b, grad.f_b, vX0, verbose=FALSE, dGamma=1, va=va, vb=vb)
 arrows(x0=vX0[1], y0=vX0[2], x1=vX_star[1], y1=vX_star[2], length=0.05, col="blue")
 vX_old<-vX_star
 
 #gamma_h=1/10
 vX_star<-ascent(f_b, grad.f_b, vX0=vX_old, verbose=FALSE, dGamma=1/10, va=va, vb=vb) #hot start: take the initial value equal to the last best guess
 arrows(x0=vX_old[1], y0=vX_old[2], x1=vX_star[1], y1=vX_star[2], length=0.05, col="blue") 
 vX_old<-vX_star
 
 #gamma_h=1/100
 vX_star<-ascent(f_b, grad.f_b, vX0=vX_old, verbose=FALSE, dGamma=1/100, va=va, vb=vb)
 arrows(x0=vX_old[1], y0=vX_old[2], x1=vX_star[1], y1=vX_star[2], length=0.05, col="blue") 
 vX_old<-vX_star
 
 # We can combine the guess to a function:
  barrier_ascent<-function(f_b, grad.f_b, vX0, epsilon_h = 1e-9, h.max = 100, verbose_ = TRUE, ...){
   #first iteration
   vXb <- ascent(f_b, grad.f_b, vX0, verbose=FALSE, dGamma=1, ...)
   vXb_old <- vX0
   h <- 1
   while( sum(abs(vXb - vXb_old)) > epsilon_h && h < h.max){
     vXb_old <- vXb
     vXb <- ascent(f_b, grad.f_b, vXb_old, verbose=FALSE, dGamma=10^(-h), ...) #gamma_h gets 10x smaller with every iteration
     if(verbose_){
       cat("at iteration", h, "the coordinates of x are", vXb, "\n")
     }
     h <- h + 1
   }
   return(vXb)
 }
 
