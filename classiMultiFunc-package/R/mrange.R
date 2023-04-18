#' mrange
#' 
#' Range distance between multivariate trajectories
#'
#' Computes the Range distance for all pairs of \eqn{m}-dimensional trajectories For a single pair of functions,
#' the current R function returns the absolute difference between the ranges of \eqn{m}-dimensional trajectories.
#'
#' x = replicate ( 4 , rnorm ( 100 , 0 , 3 ) )
#' y = replicate ( 4 , rnorm ( 100 , 5 , 3 ) )
#' data = list ( x = x, y = y )
#' mrange ( data , parallel = FALSE , cl = NULL )
#'
#' @export

mrange = function ( data , parallel = FALSE , cl = NULL ) 
{ 
  checkmate :: assertList( data )
  checkmate :: assertLogical ( parallel )
  if ( !is.null ( cl ) ) {
    if ( !isTRUE ( parallel ) ) {
      stop ( "Error: parallel should be TRUE" ) }
    checkmate :: assertNumeric ( cl , lower = 1 ) }
  d = length ( data ) 
  if ( sum ( apply ( sapply ( data , dim ) , 1 , diff ) ) != 0 ) {
    stop("Error: objects in data have different dimensions" ) }
  if ( sum ( sapply (data , function ( x ) sum ( is.na ( x ) ) ) ) != 0 ) {
    warning ( "data have missing values; some distances cannot be computed" ) }
  
  n = unique ( sapply ( data , ncol ) )
  if ( parallel == FALSE ) {
    return ( matrix ( apply ( cbind ( 1 : n ) , 1 ,
                              function ( i ) sqrt ( colSums (
                                ( t ( sapply ( data ,
                                               function ( x ) apply ( x , 2 , function ( j ) abs ( max ( j ) - min ( j ) ) ) ) ) - 
                                    t ( sapply ( data ,
                                                 function ( x ) apply ( x , 2 , function ( j ) abs ( max ( j ) - min ( j ) ) ) ) ) [,i] )^2 ) ) ) ,
                      ncol = n , nrow = n , byrow = TRUE ) ) 
  } else {
    cl = parallel :: makeCluster ( cl )
    parallel :: clusterExport ( cl ,
                                list ( "data" ) )
    return ( matrix ( parallel :: parRapply ( cl = cl ,
                                              cbind ( 1 : n ) , 
                                              function ( i ) sqrt ( colSums (
                                                ( t ( sapply ( data ,
                                                               function ( x ) apply ( x , 2 , function ( j ) abs ( max ( j ) - min ( j ) ) ) ) ) - 
                                                    t ( sapply ( data ,
                                                                 function ( x ) apply ( x , 2 , function ( j ) abs ( max ( j ) - min ( j ) ) ) ) ) [,i] )^2 ) ) ) ,
                      ncol = n , nrow = n , byrow = TRUE ) )
    parallel :: stopCluster ( cl ) }
}

