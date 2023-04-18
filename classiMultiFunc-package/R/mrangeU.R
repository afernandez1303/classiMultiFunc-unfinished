#' mrangeU
#' 
#' Range distance between multivariate trajectories
#'
#' Computes the Range distance for all pairs of \eqn{m}-dimensional trajectories For a single pair of functions,
#' the current R function returns the absolute difference between the ranges of \eqn{m}-dimensional trajectories.
#'
#' x = replicate ( 4 , rnorm ( 100 , 0 , 3 ) )
#' mrangeU ( x , parallel = FALSE , cl = NULL )
#'
#' @export
mrangeU = function ( data , parallel = FALSE , cl = NULL ) 
{ 
  checkmate :: assertMatrix ( data )
  checkmate :: assertLogical ( parallel )
  if ( !is.null ( cl ) ) {
    if ( !isTRUE ( parallel ) ) {
      stop ( "Error: parallel should be TRUE" ) }
    checkmate :: assertNumeric ( cl , lower = 1 ) }
  d = length ( data ) 
  if ( sum ( sapply (data , function ( x ) sum ( is.na ( x ) ) ) ) != 0 ) {
    warning ( "data have missing values; some distances cannot be computed" ) }
  
  n = ncol ( data )
  if ( parallel == FALSE ) {
    range = apply ( data , 2 , function ( x ) abs ( max ( x ) - min ( x ) ) ) 
    combs_lower_tri = utils :: combn ( 1 : n , 2 , simplify = FALSE )
    lower_tri_res = sapply ( combs_lower_tri , 
                             function ( x ) abs ( range [ x [[ 1 ]] ] - range [ x [[ 2 ]] ] )  ) # compute results for upper triangular distance matrix
  } else {
    cl = parallel :: makeCluster ( cl )
    parallel :: clusterExport ( cl , list ( "range" ) )
    lower_tri_res = parallel::parSapply(cl, 
                                        combs_lower_tri,
                                        function ( x ) abs ( range [ x [[ 1 ]] ] - range [ x [[ 2 ]] ] ) ) # compute results for upper triangular distance matrix
    
    parallel :: stopCluster ( cl ) }
  
  # format results
  result_matrix = matrix ( NA , nrow = n, ncol = n )
  result_matrix [ lower.tri ( result_matrix ) ] = lower_tri_res
  result_matrix = t ( result_matrix )
  result_matrix [ lower.tri ( result_matrix ) ] = lower_tri_res
  diag ( result_matrix ) = rep ( 0 , n ) 
  return ( result_matrix )
}

