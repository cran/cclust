count <- function(x)
  {
    nr <- nrow(x)
    nc <- ncol(x)
    d <- integer(nc+1)

    retval <- .C("count", xrows=nr, xcols=nc, x=as.integer(x), d=d)

    d <- retval$d
    
    names(d) <- 0:nc
    return(d)
  }

      
        
