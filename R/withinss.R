withinss <- function(clobj, x){
  
    retval <- rep(0, nrow(clobj$centers))
    x <- (x - clobj$centers[clobj$cluster, ])^2
    for(k in 1:nrow(clobj$centers)){
        retval[k] <- sum(x[clobj$cluster==k,])
    }
    retval
}



