calinski <- function(zgss, clsize)
  {
    n <- sum(clsize)
    k <- length(clsize)
    vrc <- (zgss$bgss/(k-1))/(zgss$wgss/(n-k))
    return(vrc=vrc)
  }

cindex <- function (withins, minmaxd, clsize)
  {
    dw <- sum(withins*clsize)    
    ##c-index
    cindex <- (dw -minmaxd$mindw)/(minmaxd$maxdw - minmaxd$mindw)
    return(cindex)
  }



db <- function(withins, centers, cluster)
  {
    mse <- withins/table(cluster)
    r <- outer(mse, mse, "+") / as.matrix(dist(centers, diag=TRUE))
    diag(r) <- 0
    db <- mean(apply(r,1,max))
    return(db)
  }


hartigan <- function(zgss)
  {
    hart <- log(zgss$bgss/zgss$wgss)
    return(hart)
  }


ratkowsky <- function(zvargss, clsize)
  {
    k <- length(clsize)
    ratio<-mean(sqrt(zvargss$varbgss/zvargss$vartss))
    rat <- ratio/sqrt(k)
    return(rat)
  }


scott <- function(zttw, clsize)
  {
    n <- sum(clsize)
    dettt<-prod(eigen(zttw$tt)$values)
    detw<-prod(eigen(zttw$w)$values)
    scott <- n * log(dettt/detw)
    return(scott)
  }

marriot <- function(zttw, clsize)
  {
    k <- length(clsize)
    detw<-prod(eigen(zttw$w)$values)
    mar <- (k**2) * detw
    return(mar)
  }


ball <- function(withins, clsize)
  {
    ball <- sum(withins)/length(clsize)
  }


tracecovw <- function(zttw)
  {
    trcovw <- sum(diag(cov(zttw$w)))
    return(trcovw)
  }


tracew <- function(zttw)
  {
    tracew <- sum(diag(zttw$w))
    return(tracew)
  }


friedman <- function(zttw)
  {
    b <- zttw$tt-zttw$w
    fried <- sum(diag(solve(zttw$w)%*%b))
    return(fried)
  }


rubin <- function(zttw)
  {
    dettt<-prod(eigen(zttw$tt)$values)
    detw<-prod(eigen(zttw$w)$values)
    friedm <- dettt/detw
    return(friedm)
  }


ssi <- function (centers, clsize)
  {
    ncl <- dim(centers)[1]
    nvar <- dim(centers)[2]
    n <- sum(clsize)

    cmax <- apply(centers, 2, max)
    cmin <- apply(centers, 2, min)
    cord <- apply(centers, 2, order)
    cmaxi <- cord[ncl,]
    cmini <- cord[1,]

    meanmean <- mean(centers)
    absmdif <- abs(apply(centers, 2, mean) - meanmean)
    span <- cmax - cmin
    csizemax <- clsize[cmaxi]
    csizemin <- clsize[cmini]

    hiest <- nvar
    hiestw <- hiest * max(max(csizemax), max(csizemin)) * exp(-min(absmdif))

    sist <- sum(span)/hiest

    sistw <- (span * exp(-absmdif)) %*% sqrt(csizemax*csizemin) / hiestw

    return(list(ssi=sist, ssiw=sistw))
  }

likelihood <- function (x, centers, cluster)
  {
    n <- nrow(x)
    l <- 0

    for (i in 1:n)
      l <- l - log(prod(x[i,]*centers[cluster[i],] +
                    (1-x[i,])*(1-centers[cluster[i],])))
    
    return(l)
  }
