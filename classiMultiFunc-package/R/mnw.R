#' mnw
#' 
#' Needleman Wunsch 
#' 
#' @export
#' 

  mnw = function ( data , parallel = FALSE, cl = NULL , method = "levenshtein" ) 
{
  checkmate :: assertMatrix ( data )
  checkmate :: assertLogical ( parallel )
  checkmate :: assertChoice ( method , choices = c( "levenshtein" , "hamming" ) ) 
  
  if ( !is.null ( cl ) ) {
    if ( !isTRUE ( parallel ) ) {
      stop ( "Error: parallel should be TRUE" ) }
    checkmate :: assertNumeric ( cl , lower = 1 ) }
  if ( sum ( is.na ( data )  != 0 ) ) {
    warning ( "data have missing values; some distances cannot be computed." ) }
  n = ncol ( data )
  combs_lower_tri = utils :: combn ( 1 : n , 2 , simplify = FALSE )
  
  if ( parallel == FALSE ) {
    lower_tri_res = sapply ( combs_lower_tri , 
                             function ( x ) as.numeric ( MKomics :: stringDist ( data [ , x [ 1 ] ] , 
                                                                                 data [ , x [ 2 ] ] , 
                                                                                 method = method ) ) )
  } else {
    cl = parallel :: makeCluster ( cl )
    parallel :: clusterExport ( cl , 
                                list ( "data" , "method" ) ,
                                envir = environment ( ) )
    parallel :: clusterEvalQ ( cl , library ( MKomics ) )
    lower_tri_res = parallel :: parSapply ( cl , 
                                            combs_lower_tri,
                                            function ( x ) as.numeric ( MKomics :: stringDist ( data [ , x [ 1 ] ] , 
                                                                                                data [ , x [ 2 ] ] , 
                                                                                                method = method ) ) )
    parallel :: stopCluster ( cl )
  }
  
  #format results
  result_matrix = matrix ( NA , nrow = n, ncol = n )
  result_matrix [ lower.tri ( result_matrix ) ] = lower_tri_res
  result_matrix = t ( result_matrix )
  result_matrix [ lower.tri ( result_matrix ) ] = lower_tri_res
  diag ( result_matrix ) = rep ( 0 , n ) 
  return ( result_matrix )
}
