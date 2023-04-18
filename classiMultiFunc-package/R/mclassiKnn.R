#' mclassiKnn
#' 
#' Create mclassiKnn Object
#'
#' The present function creates an object to specify the k-nearest-neighbor classification
#' for \eqn{n}-different observations of \eqn{m}-dimensional functionnal data.
#'
#'
#' @param classes  \code{factor} or \code{numeric}. A vector containing the true classes of the training data.
#' @param fdata the training covariates as a \code{list} of \eqn{m} objects in \code{matrix} form. Each matrix stores a dimension of the set of functions, such that columns are individuals (\eqn{n}) and rows are discrete-time points (\eqn{t}). Values of functions should be of the same time points.
#' @param mdist \code{matrix}. A customized matrix of distances. Default is NULL. 
#' @param metric \code{string} specifying the metric for knn. The complete list of available metrics is returned by
#' \code{mdistMeasures()}.
#' @param knn \code{integer} specifying the number of nearest neighbours considered in the knn algorithm.
#' @param nderiv \code{integer} number of derivatives of the trajectory.
#' @param parallel logical value indicating whether computations should be parallelized. Default is FALSE. If TRUE, parallelization is conducted with \href{https://www.rdocumentation.org/packages/parallel}{parallel} package.
#' @param cl a cluster object created by \href{https://www.rdocumentation.org/packages/parallel}{parallel}. Default is NULL.
#' @param diag is a logical value indicating whether the diagonal of the distance matrix should be printed. Only applicable if distances of one set of functions with itself is computed (\code{isnull(y)=TRUE}). If TRUE, diagonal values are NA.
#' @param upper is a logical value indicating whether the upper triangle of the matrix should be printed. Only applicable if distances of one set of functions with itself is computed (\code{isnull(y)=TRUE}). If TRUE, upper triangle values are NA.
#' @param set.seed if a seed wants to be specified. Default is NULL. 
#' @param ... further arguments, passed to other methods.  
#' @return Returns a list containing the parameters and the training data for a mknn prediction. This list is used for prediction with the function \code{\link[classiMultiFuncTest:predict.mclassiKnn]{predict.mclassiKnn}}
#'
#' @seealso See \code{\link[parallel:makeCluster]} \code{\link[parallel:makeCluster]{makeCluster}}, \code{\link[parallel:clusterExport]{clusterExport}}, \code{\link[parallel:stopCluster]{stopCluster}}, \code{\link[parallel:parApply]{parApply}} and \code{\link[parallel:parLapply]{parLapply}} from  \href{https://www.rdocumentation.org/packages/parallel}{parallel}
#' @export

mclassiKnn = function ( classes ,
                        fdata ,
                        mdist = NULL ,
                        metric = "Euclidean" , 
                        knn = 1L ,
                        nderiv = 0L ,
                        parallel = FALSE ,
                        cl = NULL ,
                        diag = FALSE ,
                        upper = FALSE ,
                        set.seed = NULL , 
                        ... )
  {
  if ( is.numeric ( classes ) ) {
    classes = as.factor ( classes ) }
  checkmate :: assertFactor ( classes )
  checkmate :: assertDataFrame ( fdata )
  d = unique ( fdata [ , "dim" ] )
  fdata = t ( fdata [ , - which ( names ( fdata ) %in% c ( "id" , "dim" ) ) ] )
  fdata = plyr :: alply ( cbind ( 1 : d ) , 
                          1 , 
                          function ( i ) cbind ( data.frame ( dim = rep ( 1 : d , each = nrow ( fdata ) / d ) , fdata ) [ data.frame ( dim = rep ( 1 : d , each = nrow ( fdata ) / d ) , fdata ) [ , "dim" ] == i , - 1 ] ) )
  checkmate :: assertList ( fdata )
  
  if ( sum ( apply ( sapply ( fdata , dim ) , 
                     1 , 
                     diff ) ) != 0 ) {
    stop ( "Error: objects in fdata have different dimensions" ) }
  checkmate :: assertIntegerish ( knn , 
                                  lower = 1L , 
                                  len = 1 )
  checkmate :: assertIntegerish ( nderiv , 
                                  lower = 0L )
  checkmate :: assertCharacter ( metric )
  checkmate :: assertChoice ( metric , 
                              choices = mmetricChoices ( ) )
  checkmate :: assertLogical ( parallel )
  if ( !is.null ( cl ) ) {
    if ( !isTRUE ( parallel ) ) {
      stop ( "Error: parallel should be TRUE" ) }
    checkmate :: assertNumeric ( cl , 
                                 lower = 1 ) }
  nret = list ( classes = classes ,
                fdata = fdata ,
                mdist = mdist ,
                knn = knn ,
                metric = metric ,
                nderiv = nderiv ,
                parallel = parallel ,
                cl = cl ,
                diag = diag ,
                upper = upper ,
                set.seed = set.seed )
  # class ( nret ) = "mclassiKnn"
  return ( nret )
  } 

# TO DO:
# data transformation into funcional data / data normalization. This can be done in the prediction, 
# but a parameter is needed to specify this 


