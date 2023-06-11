# Following gives the median for each column in mX without using R commando
MyMedian <- function(mX){
  for (i in 1:iN){
        vSort <- sort(mX[,i])
        vStorage[i] <- ifelse(iT%%2==1,mSort[(iT+1)/2],mean(vSort[iT/2+0:1]))
    }
    return(vStorage)
}
