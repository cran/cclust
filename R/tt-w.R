ttww <- function(x, clsize, cluster)
  {
    n <- sum(clsize)
    k <- length(clsize)
    w<-0
    tt <- cov(x)*n
    for (l in 1:k)
      w<- w+cov(x[cluster==l,])*clsize[l]
    zttw <- list(tt=tt, w=w)
    return(zttw)
  }

