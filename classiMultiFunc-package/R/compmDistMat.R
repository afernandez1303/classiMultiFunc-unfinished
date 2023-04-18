#' compmDistMat
#' 
#' Distance matrix among \eqn{m}-dimensional functions/trajectories
#'
#' The present function computes the distance matrix of one set of \eqn{n} different \eqn{m}-dimensional functions to another set of \eqn{k} different \eqn{m}-dimensional functions. The distances for all combinations of functions in the first set with functions of the second set are returned.
#' Different distance measures are available and can be set by \code{method}.
#'
#' @param x \code{list} of \eqn{m} objects in matrix form. Each matrix stores a dimension of the set of functions, such that columns are individuals (\eqn{n}) and rows are discrete-time points (\eqn{t}). Values of functions should be of the same time points.
#' @param y \code{list} of \eqn{m} objects in matrix form. Each matrix stores a dimension of the set of functions, such that columns are individuals (\eqn{k}) and rows are discrete-time points (\eqn{t}). Values of functions should be of the same time points.
#' @param method \code{string} to specify the distance measure to compute. Defaults to "Euclidean". The complete list of available options is returned by \code{mdistMeasures()}.
#' @param parallel logical value indicating whether computations should be parallelized. Default is FALSE. If TRUE, parallelization is conducted with \href{https://www.rdocumentation.org/packages/parallel}{parallel} package.
#' @param cl a cluster object created by \href{https://www.rdocumentation.org/packages/parallel}{parallel}. Default is NULL.
#' @param diag is a logical value indicating whether the diagonal of the distance matrix should be printed. Only applicable if distances of one set of functions with itself is computed (\code{isnull(y)=TRUE}). If TRUE, diagonal values are NA.
#' @param upper is a logical value indicating whether the upper triangle of the matrix should be printed. Only applicable if distances of one set of functions with itself is computed (\code{isnull(y)=TRUE}). If TRUE, upper triangle values are NA.
#' @param ... further arguments, passed to other methods. 
#' @return The present function returns a rectangular matrix of size \eqn{k*n} containing the distances of
#' all \eqn{k} functions in \code{y} with all n functions/trajectories in \code{x}. If \code{y} is left unspecified, the function returns the distances of the functions in \code{x} with itself (a symmetric \eqn{n x n} matrix).
#' @seealso See \code{\link[parallel:makeCluster]} \code{\link[parallel:makeCluster]{makeCluster}}, \code{\link[parallel:clusterExport]{clusterExport}}, \code{\link[parallel:stopCluster]{stopCluster}}, \code{\link[parallel:parApply]{parApply}} and \code{\link[parallel:parLapply]{parLapply}} from  \href{https://www.rdocumentation.org/packages/parallel}{parallel}.
#' @export
#' 

compmDistMat=function( x ,
                       y = NULL ,
                       method = "Euclidean" ,
                       parallel = FALSE ,
                       cl = NULL ,
                       diag = TRUE ,
                       upper = TRUE , 
                       ... )
  {
  
  # Block 1: overall checks
  checkmate::assertList ( x )
  data = x
  d = length ( data )
  checkmate :: assertLogical ( diag )
  checkmate :: assertLogical ( upper )
  checkmate :: assertLogical ( parallel )
  
  if ( !is.null ( cl ) ) {
    if ( !isTRUE ( parallel ) ) {
      stop ( "Error: parallel should be TRUE" ) }
    checkmate :: assertNumeric ( cl , lower = 1 ) }
  
  # Block 2: list of objects (x: if there is not testing set, x and y if there is a testing set)
  if ( ! is.null ( y ) ) {
    checkmate :: assertList ( y )
    if ( ! ( length ( x ) == length ( y ) ) ) {  # check if the components have the same number of individuals and time-points
      stop("Error: x and y should have the same length" ) }
    allist = list ( x , y )
    data = plyr :: alply ( cbind ( 1 : d ) , 
                           1 , 
                           function ( i ) do.call ( cbind , 
                                                    do.call ( cbind , allist ) [ i , ] ) ) }
  
  # Block 3: compute the distance depending on method or measure
  source ( paste ( paste ( paste ( "m" , "metric" , sep = "" ) , 
                             "Choices" , sep = "" ) ,
                     "R" , sep = "." ) )
  checkmate :: assertChoice ( method , 
                              choices = classiMultiFuncTest :: mmetricChoices ( ) )
  if ( method %in% unlist ( sapply ( proxy :: pr_DB$get_entries ( ) [ c ( "Euclidean" ,
                                                                          "Manhattan" , 
                                                                          "Minkowski")] , 
                                     function ( x ) x$names ) ) ) {
    method = names ( sapply ( sapply ( proxy::pr_DB$get_entries ( ) , 
                                         function ( x ) x$names ) , 
                              function ( x ) sum ( x == method ) ) [ sapply ( sapply ( proxy :: pr_DB$get_entries ( ) , 
                                                                                       function ( x ) x$names ) ,
                                                                              function ( x ) sum ( x == method ) ) == 1 ] ) }
    source ( paste ( paste ( "m" , method , sep = "" ) , "R" , sep = "." ) )
    mdist = do.call ( paste ( "m" , method , sep = "" ) , 
                      list ( data , 
                             parallel = parallel , 
                             cl = cl ) ) 
    
  # Block 4: prepare the output
  if ( upper == TRUE ) {
    if ( diag == FALSE ) {
      diag ( mdist ) = NA } }
    if ( upper == FALSE ) {
      mdist [ upper.tri ( mdist , 
                          diag =! diag ) ] = NA }
    rownames ( mdist ) = paste ( "x" , 1 : nrow ( mdist ) , sep = "-" )
    colnames ( mdist ) = paste ( "x" , 1 : nrow ( mdist ) , sep = "-" )
    if ( !is.null ( y ) ) {
      mdist = mdist [ ( ncol ( x [[ 1 ]] ) + 1 ) : ( ncol ( mdist ) ) , 
                      1 : ncol ( x [[ 1 ]] ) ]
      if ( ncol ( y [[ 1 ]] ) != 1 ) {
        rownames ( mdist ) = paste ( "y" , 
                                     1 : nrow ( mdist ) ,
                                     sep = "-" ) } }
    return ( mdist )
    }

# TO DO(s):
# Add the option of weight distances 
# NAs -> distances give error if missing. What if we have NA(s)? 
