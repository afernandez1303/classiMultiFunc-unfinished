% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mglobMaxU.R
\name{mglobMaxU}
\alias{mglobMaxU}
\title{mglobMaxU}
\usage{
mglobMaxU(data, parallel = FALSE, cl = NULL)
}
\arguments{
\item{data}{a matrix that stores a dimension of the set of functions, such that columns are individuals (\eqn{n}) and rows are discrete-time points (\eqn{t}). Functions' values should be of the same time points.}

\item{parallel}{logical value indicating whether computations should be parallelized. Default is FALSE. If TRUE, parallelization is conducted with \href{https://www.rdocumentation.org/packages/parallel}{parallel} package.}

\item{cl}{a cluster object created by \href{https://www.rdocumentation.org/packages/parallel}{parallel}. Default is NULL.}
}
\value{
Returns a square and symmetric \eqn{n x n} matrix of \eqn{m}-dimensional global maximum distances.
}
\description{
Global Maximum distance between univariate functions
}
\details{
Computes the Global Maximum distance for all pairs of \eqn{m}-dimensional functions. For a single pair of functions,
the present R function returns the maximum euclidean distance between the function values at equal time points.

For each pair of functions f and g, the present R function computes: \eqn{max t [Euclidean_Distance(f(t), g(t))]}
}
\examples{

## 2-dimensional functions

x = replicate ( 4 , rnorm ( 100 , 0 , 3 ) )
mglobMaxU ( x , parallel = FALSE , cl = NULL )

}
\seealso{
See \code{\link[parallel:makeCluster]{makeCluster}}, \code{\link[parallel:clusterExport]{clusterExport}}, \code{\link[parallel:stopCluster]{stopCluster}}, \code{\link[parallel:parApply]{parApply}} and \code{\link[parallel:parLapply]{parLapply}} from  \href{https://www.rdocumentation.org/packages/parallel}{parallel}, and \code{\link[proxy:dist]{dist}} from \href{https://cran.r-project.org/web/packages/proxy/index.html}{proxy}
}
