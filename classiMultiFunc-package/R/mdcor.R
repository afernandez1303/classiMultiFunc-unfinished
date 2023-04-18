#' mdcor 
#' 
#' mdcor distance between multivariate trajectories
#'
#' Computes the dcor distance for all pairs of \eqn{m}-dimensional trajectories.
#' @param  data a list of \eqn{m} objects in matrix form. Each matrix stores a dimension of the set of functions, such that columns are individuals (\eqn{n}) and rows are discrete-time points (\eqn{t}). Values of functions should be of the same time points.
#' @param parallel logical value indicating whether computations should be parallelized. Default is FALSE. If TRUE, parallelization is conducted with \href{https://www.rdocumentation.org/packages/parallel}{parallel} package.
#' @param cl a cluster object created by \href{https://www.rdocumentation.org/packages/parallel}{parallel}. Default is NULL.
#  @param index Default is 1. 
#' @export

mdcor = function ( data , parallel = FALSE , cl = NULL , index = 1.0 ) 
{ 
  checkmate :: assertList( data )
  checkmate :: assertLogical ( parallel )
  if ( !is.null ( cl ) ) {
    if ( !isTRUE ( parallel ) ) {
      stop ( "Error: parallel should be TRUE" ) }
    checkmate :: assertNumeric ( cl , lower = 1 ) }
  if ( sum ( apply ( sapply ( data , dim ) , 1 , diff ) ) != 0 ) {
    stop("Error: objects in data have different dimensions" ) }
  if ( sum ( sapply (data , function ( x ) sum ( is.na ( x ) ) ) ) != 0 ) {
    warning ( "data have missing values; some distances cannot be computed" ) }
  
  n = unique ( sapply ( data , ncol ) )
  t = unique ( sapply ( data , nrow ) )
  d = length ( data )
  data_array = array ( unlist ( data ) , 
                       dim = c ( t , n , d ) ) 
  combsLowerTri = combn ( 1 : n , 2 , simplify = FALSE ) 
  if ( parallel == FALSE ) {
    lower_tri_res = sapply ( combsLowerTri ,
                             function ( x ) ( 1 - energy :: dcor ( data_array [ , x [ 1 ] , ] ,
                                                                   data_array [ , x [ 2 ] , ] ,
                                                                   index = index ) ) )
    
    
  } else {
    cl = parallel :: makeCluster ( cl )
    parallel :: clusterExport ( cl , 
                                list ( "data" , "index" ) , 
                                envir = environment ( ) )
    parallel :: clusterEvalQ ( cl , library ( energy ) )
    lower_tri_res = parallel :: parSapply ( cl , 
                                            combsLowerTri ,
                                            function ( x ) ( 1 - energy :: dcor ( data_array [ , x [ 1 ] , ] ,
                                                                                  data_array [ , x [ 2 ] , ] ,
                                                                                  index = index ) ) ) 
    parallel :: stopCluster ( cl ) }
  
  #arrange results into distance matrix
  resultMat = matrix ( NA , nrow = n , ncol = n )
  resultMat [ lower.tri ( resultMat ) ] = lower_tri_res
  resultMat = t ( resultMat )
  resultMat [ lower.tri ( resultMat ) ] = lower_tri_res
  diag ( resultMat ) = rep ( 0 , n )
  return ( resultMat )
}
