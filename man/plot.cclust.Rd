\name{plot.cclust}
\title{Plot Clustered Data}
\usage{
plot(y, x, centers=TRUE, initcenters=FALSE)
}
\alias{plot.cclust}
\arguments{
 \item{y}{Object of class \code{"cclust"} returned by a clustering algorithm such as \code{\link{cclust}}}
 \item{x}{Data matrix where columns correspond to variables and rows to observations}
 \item{centers}{Mark cluster centers with \code{"X"} (for 2-dimensional
   data only)}
 \item{initcenters}{Mark initial cluster centers with \code{"+"} (for
   2-dimensional data only)}}
}
\description{
  Plot the data using different colors for each cluster. If \code{x} has
  more than 2 columns, a \code{\link{pairs}} plot is produced.
}
\author{Friedrich Leisch and Andreas Weingessel}
\seealso{\code{\link{cclust}}, \code{\link{predict.cclust}}}}
\examples{
# a 2-dimensional example
x<-rbind(matrix(rnorm(100,sd=0.3),ncol=2),
         matrix(rnorm(100,mean=1,sd=0.3),ncol=2))
cl<-cclust(x,2,20,verbose=TRUE,method="kmeans")
plot(cl,x)   

# a 3-dimensional example
x<-rbind(matrix(rnorm(150,sd=0.3),ncol=3),
         matrix(rnorm(150,mean=1,sd=0.3),ncol=3),
         matrix(rnorm(150,mean=2,sd=0.3),ncol=3))
cl<-cclust(x,6,20,verbose=TRUE,method="kmeans")
plot(cl,x)

# assign classes to some new data
y<-rbind(matrix(rnorm(33,sd=0.3),ncol=3),
         matrix(rnorm(33,mean=1,sd=0.3),ncol=3),
         matrix(rnorm(3,mean=2,sd=0.3),ncol=3))
ycl<-predict(cl, y)
plot(ycl,y)
}
\keyword{cluster}







