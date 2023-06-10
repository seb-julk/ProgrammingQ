  # 10.1 Penalization functions ----
 #penalizion function
 p<-function(vX, va, vb){
   gX<-c(vX-vb, va-vX)
   return(sum(max(gX,0))^2)
 }
 
 #new penalized objective
 f_p<-function(vX, dGamma, ...){
   return(f(vX)-dGamma*p(vX, ...)) #subtract the penalty since we're maximizing
 }
 
 # We need the derivative. Here we just use code but THIS MIGHT NEED TO BE CHANGED!
  library(numDeriv)
 grad.f_p<-function(vX, dGamma, ...){
   return(grad(func=f_p, x=vX, dGamma=dGamma, ...)) # using numerical derivatives here because I'm lazy
 }
 
 # We can then write the function that iterates. Further down we can see the iterations provided in plots
 #making a function that iterates like above
 penalized_ascent<-function(f_p, grad.f_p, vX0, epsilon_h = 1e-9, h.max = 100, verbose_ = TRUE, ...){
   #first iteration
   vXh <- ascent(f_p, grad.f_p, vX0, verbose=FALSE, dGamma=0, ...)
   vXh_old <- vX0
   h <- 1
   while( sum(abs(vXh - vXh_old)) > epsilon_h && h < h.max){
     vXh_old <- vXh
     vXh <- ascent(f_p, grad.f_p, vXh_old, verbose=FALSE, dGamma=10^(h-1), ...) #gamma_h gets 10x bigger with every interation
     if(verbose_){
       cat("at iteration", h, "the coordinates of x are", vXh, "\n")
     }
     h <- h + 1
   }
   return(vXh)
 }
 
 
 
 # The iteration in steps
 #Plotting
 {vx1 <- seq(-0.5, 3, 0.05)
   vx2 <- seq(-0.5, 2, 0.05)
   mf <- matrix(0, length(vx1), length(vx2))
   for(i in 1:length(vx1)){
     for(j in 1:length(vx2)){
       mf[i,j]<-f(c(vx1[i], vx2[j]))
     }
   }
   {
     plot(NA,xlim=range(vx1),
          ylim=range(vx2),xlab=expression("x"[1]),ylab=expression("x"[2]),
          frame=FALSE)
     levels = pretty(range(mf), 50)
     color.palette = function(n) hcl.colors(n, "YlOrRd", rev = TRUE)
     .filled.contour(x=vx1, y=vx2, z=mf,
                     levels=levels,
                     col=color.palette(length(levels) - 1))
   }
   
   
   
   #choose upper and lower bounds va and vb, and draw the corresponding box constraint
   #this box doesn't contain any local minimum, so any solution will be on the border
   va=c(1,0.5)
   vb=c(2.5,1)
   rect(xleft=va[1], ybottom=va[2], xright=vb[1], ytop=vb[2])
   
   #choose a point inside the box - penalty=0
   vX=c(1.5,0.75)
   points(vX[1],vX[2], col="blue", pch=16)
   p(vX, va, vb)
   
   #point outside -  penalty>0
   vX=c(0.5,0.75)
   points(vX[1],vX[2], col="green", pch=16)
   p(vX, va, vb)
   
   
   
   #plotting the solutions for different gamma_h
   {
     plot(NA,xlim=range(vx1),
          ylim=range(vx2),xlab=expression("x"[1]),ylab=expression("x"[2]),
          frame=FALSE)
     levels = pretty(range(mf), 50)
     color.palette = function(n) hcl.colors(n, "YlOrRd", rev = TRUE)
     .filled.contour(x=vx1, y=vx2, z=mf,
                     levels=levels,
                     col=color.palette(length(levels) - 1))
     rect(xleft=va[1], ybottom=va[2], xright=vb[1], ytop=vb[2])
   }
   
   
   vX0<-c(1.5,1.5)
   points(vX0[1],vX0[2], col="blue", pch=16)
   
   
   #gamma_h=0, no penalization
   vX_star<-ascent(f_p, grad.f_p, vX0, verbose=FALSE, dGamma=0, va=va, vb=vb)
   arrows(x0=vX0[1], y0=vX0[2], x1=vX_star[1], y1=vX_star[2], length=0.05, col="blue")
   vX_old<-vX_star
   
   #gamma_h=1 
   vX_star<-ascent(f_p, grad.f_p, vX0=vX_old, verbose=FALSE, dGamma=1, va=va, vb=vb) #hot start: take the initial value equal to the last best guess
   arrows(x0=vX_old[1], y0=vX_old[2], x1=vX_star[1], y1=vX_star[2], length=0.05, col="blue") 
   vX_old<-vX_star
   
   #gamma_h=10 
   vX_star<-ascent(f_p, grad.f_p, vX0=vX_old, verbose=FALSE, dGamma=10, va=va, vb=vb)
   arrows(x0=vX_old[1], y0=vX_old[2], x1=vX_star[1], y1=vX_star[2], length=0.05, col="blue") 
   vX_old<-vX_star
   
   #gamma_h=100
   vX_star<-ascent(f_p, grad.f_p, vX0, verbose=FALSE, dGamma=100, va=va, vb=vb)
   arrows(x0=vX_old[1], y0=vX_old[2], x1=vX_star[1], y1=vX_star[2], length=0.05, col="blue")
   
 }
 
