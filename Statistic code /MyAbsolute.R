Find the absolute values for each column in a matrix

myAbs <- function(iT, iN, mX){
mAbs <- matrix(NA, nrow = iT, ncol = iN)
    for (i in 1:iN){
        for (t in 1:iT){
            if (mX[t,i] >= 0){
                mAbs[t,i] <- mX[t,i] 
            }
            else{
                mAbs[t,i] <- -1 * mX[t,i]
            }
        }
    }
    return(mAbs)
}
