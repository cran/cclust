gss <- function(x, clsize, withins)
  {
    n <- sum(clsize)
    k <- length(clsize)
    allmean <- apply(x,2,mean)
    dmean <- sweep(x,2,allmean,"-")
    allmeandist <- sum(dmean^2)
    wgss <- sum(withins)
    bgss <- allmeandist - wgss
    
    zgss <- list(wgss=wgss, bgss=bgss)
    return(zgss)
  }





