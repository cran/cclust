maxmindist <-  function (clsize, distscen)
  {
    ##only for binary data

     ncl <- length(clsize)
       
    npairs <- 0
    for (i in 1:ncl)
      npairs <- npairs + clsize[i]*(clsize[i]-1)/2
   
    ##minimum distance
    mindw <- 0
    nfound <- distscen[1]
    i <- 1
    while (nfound < npairs)
      {
        if ((nfound+distscen[i+1]) < npairs)
          {
            mindw <- mindw + i*distscen[i+1]
            nfound <- nfound+distscen[i+1]
          }
        else
          {
            mindw <- mindw + i*(npairs-nfound)
            nfound <- nfound+distscen[i+1]
          }
        i <- i+1
      }
    
    ##maximum distance
    maxdw <- 0
    nfound <- 0
    i <- length(distscen) - 1
    while (nfound < npairs)
      {
        if ((nfound+distscen[i+1]) < npairs)
          {
            maxdw <- maxdw + i*distscen[i+1]
            nfound <- nfound+distscen[i+1]
          }
        else
          {
            maxdw <- maxdw + i*(npairs-nfound)
            nfound <- nfound+distscen[i+1]
          }
        i <- i-1
      }
    
    minmaxd <-  list(mindw=mindw, maxdw=maxdw)
    
    return(minmaxd)
  }
