vargss <- function(x, clsize, varwithins)
  {
    nvar<-dim(x)[2]
    n <- sum(clsize)
    k <- length(clsize)
    varallmean<-rep(0,nvar)
    varallmeandist<-rep(0,nvar)
    varwgss<-rep(0,nvar)
    for (l in 1:nvar)
      varallmean[l] <- mean(x[,l])
    vardmean <- sweep(x,2,varallmean,"-")
    for (l in 1:nvar)
      {
        varallmeandist[l] <- sum((vardmean[,l])^2)
        varwgss[l] <- sum(varwithins[,l])
      }
    varbgss <- varallmeandist - varwgss
    vartss<-varbgss+varwgss
    zvargss <- list(vartss=vartss, varbgss=varbgss)
    return(zvargss)
  }
