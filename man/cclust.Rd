\name{cclust}
\title{Convex Clustering }
\usage{
cclust (x, centers, iter.max=100, verbose=FALSE, dist="euclidean",
        method= "kmeans", rate.method="polynomial", rate.par=NULL)

print.cclust(cclust.obj)
}
\alias{cclust}
\alias{kmeans}
\alias{hardcl}
\alias{neuralgas}
\alias{print.cclust}
\arguments{
\item{x}{Data matrix}
\item{centers}{Number of clusters or initial values for cluster centers}
\item{iter.max}{Maximum number of iterations}
\item{verbose}{If TRUE, make some output during learning}
\item{dist}{If "euclidean", then mean square error, if "manhattan ",
            the mean absolute error is used}
\item{method}{If "kmeans",then we have the kmeans clustering method,
              if "hardcl" we have the On-line Update (Hard Competitive
	      learning) method, and if "neuralgas", we have the Neural
	      Gas (Soft Competitive learning) method.}
\item{rate.method}{If "kmeans", then k-means learning rate, otherwise
                   exponential decaying learning rate.
                   It is used only for the Hardcl method.}
\item{rate.par}{The parameters of the learning rate.} 
}
\description{

  The data given by \code{x} is clustered by an algorithm.
      
  If \code{centers} is a matrix, its rows are taken as the initial cluster
  centers. If \code{centers} is an integer, \code{centers} rows
  of \code{x} are randomly chosen as initial values.

  The algorithm stops, if no cluster center has changed during the last
  iteration or the maximum number of iterations (given by
  \code{iter.max}) is reached.

  If \code{verbose} is TRUE, only for "kmeans" method, displays
  for each iteration the number of the iteration and the numbers
  of cluster indices which have changed since the last iteration is given.

  If \code{dist} is "euclidean", the distance between the cluster center
  and the data points is the Euclidian distance (ordinary kmeans
  algorithm). If "manhattan", the distance between the
  cluster center and the data points is the sum of the absolute values
  of the distances of the coordinates.

  If \code{method} is "kmeans",then we have the kmeans clustering
  method, which works by repeatedly moving all cluster centers
  to the mean of their Voronoi sets. If "hardcl" we have the On-line
  Update (Hard Competitive learning) method, which works by performing
  an update directly after each input signal, and if "neuralgas" we have
  the Neural Gas (Soft Competitive learning) method, that sorts for each
  input signal the units of the network according to the distance
  of their reference vectors to input signal.


  If \code{rate.method} is "polynomial", the polynomial learning rate
  is used, that means 1/t, where t stands for the number of input
  data for which a particular cluster has benn the winner so far.
  If "exponentially decaying", the exponential decaying learning rate
  is used according to par1*{(par2/par1)]^(iter/itermax) where par1
  and par2 are the initial and final values of the l.rate.
    
  The parameters \code{rate.par} of the learning rate, where
  if \code{rate.method} is "polynomial" then by default rate.par=1.0,
  otherwise rate.par=(0.5,1e-5)}.
  
}
\value{
  \code{cclust} returns an object of class "cclust".
  \item{centers}{The final cluster centers.}
  \item{initcenters}{The initial cluster centers.}
  \item{ncenters}{The number of the centers.}
  \item{cluster}{Vector containing the indices of the clusters where
      the data points are assigned to.}
  \item{size}{The number of data points in each cluster.}
  \item{iter}{The number of iterations performed.}
  \item{changes}{The number of changes performed in each iteration
      step with the Kmeans algorithm.}
  \item{dist}{The distance measure used.}
  \item{method}{The agorithm method being used.}
  \item{rate.method}{The learning rate being used by the Hardcl clustering
      method.}
  \item{rate.par}{The parameters of the learning rate.}
  \item{call}{Returns a call in which all of the arguments are
      specified by their names.}
  \item{withinss}{Returns the sum of square distances within the clusters.}
}
\author{Evgenia Dimitriadou, Friedrich Leisch and Andreas Weingessel}
\seealso{\code{\link{plot.cclust}}, \code{\link{predict.cclust}}}
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
         plot(cl,y)
}
\keyword{cclust}




