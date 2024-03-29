% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mHausdorff.R
\name{mHausdorff}
\alias{mHausdorff}
\title{mHausdorff}
\usage{
mHausdorff(data, parallel = FALSE, cl = NULL)
}
\arguments{
\item{data}{a list of \eqn{m} objects in matrix form. Each matrix stores a dimension of the set of functions, such that columns are individuals (\eqn{n}) and rows are discrete-time points (\eqn{t}). Functions' values should be of the same time points.}

\item{parallel}{logical value indicating whether computations should be parallelized. Default is FALSE. If TRUE, parallelization is conducted with \href{https://www.rdocumentation.org/packages/parallel}{parallel} package.}

\item{cl}{a cluster object created by \href{https://www.rdocumentation.org/packages/parallel}{parallel}. Default is NULL.}
}
\value{
Returns a square and symmetric \eqn{n x n} matrix of \eqn{m}-dimensional Hausdorff distances.
}
\description{
Hausdorff distance between multivariate functions
}
\details{
Computes the Hausdorff distance for all pairs of \eqn{m}-dimensional functions.

Applies \code{\link[pracma:hausdorff_dist]{pracma::hausdorff_dist}} to all pairs of \eqn{m}-dimensional functions
}
\examples{
## 2-dimensional functions

x = replicate(4, rnorm(100, 0, 3))
y = replicate(4, rnorm(100, 3, 1))
data = list(x, y)
mhausdorff(data, parallel = FALSE, cl = NULL)

## 3-dimensional functions

z = replicate(4, rpois(100, 2))
data = list(x, y, z)
mhausdorff(data, parallel = FALSE, cl = NULL)

}
\seealso{
See  \code{\link[parallel:makeCluster]{makeCluster}}, \code{\link[parallel:clusterExport]{clusterExport}}, \code{\link[parallel:stopCluster]{stopCluster}}, \code{\link[parallel:parApply]{parApply}} and \code{\link[parallel:parLapply]{parLapply}} from  \href{https://www.rdocumentation.org/packages/parallel}{parallel}, \code{\link[proxy:dist]{dist}} from \href{https://cran.r-project.org/web/packages/proxy/index.html}{proxy}
}
