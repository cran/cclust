countdist <- function(x)
  {
    nr <- nrow(x)
    nc <- ncol(x)
    d <- numeric(nc+1)

    for (i in 1:(nr-2))
      {
        y <- sweep(x[(i+1):nr,],2,x[i,],"-")
        z <- apply(abs(y), 1, sum)
        for (j in 0:nc)
          d[j+1] <- d[j+1] + sum(z==j)
      }

    ## last pair
    z <- sum(abs(x[nr,]-x[nr-1,]))
    d[z+1] <- d[z+1] + 1
    
    names(d) <- 0:nc
    return(d)
  }

countdist <- function(x)
  {
    nr <- nrow(x)
    nc <- ncol(x)
    d <- numeric(nc+1)

    for (i in 1:(nr-2))
      {
        y <- matrix(0,nr-i,nc)
        for (j in 1:nc)
          y[,j] <- x[(i+1):nr,j] - x[i,j]
        y <- abs(y)
        z <- y[,1]
        for (j in 2:nc)
          z <- z + y[,j]
        for (j in 0:nc)
          d[j+1] <- d[j+1] + sum(z==j)
      }

    ## last pair
    z <- sum(abs(x[nr,]-x[nr-1,]))
    d[z+1] <- d[z+1] + 1
    
    names(d) <- 0:nc
    return(d)
  }

      
        
