#' Aitchison distance from package robCompositions
#'
#' @param data is the object with the information related to the trajectories, i.e., number of times the mouse was in each of the areas of interest. 
#'
#' @export

mAitchison = function ( data ) 
  {
  checkmate :: assertMatrix ( data )
  if ( sum ( sapply ( data , 
                      function ( x ) sum ( is.na ( x ) ) ) ) != 0 ) {
    warning ( "data have missing values; some distances cannot be computed." ) }
  if ( sum ( data == 0 ) > 0 ) {
    data [ data == 0 ] = 0.5
    warning ( "0(s) in the data have been replaced by 0.5") }
  
  n = nrow ( data )
  
  lower_tri_res = robCompositions :: aDist ( data )
  result_matrix = matrix ( NA , nrow = n , ncol = n )
  result_matrix [ lower.tri ( result_matrix ) ] = lower_tri_res
  result_matrix = t ( result_matrix )
  result_matrix [ lower.tri ( result_matrix ) ] = lower_tri_res
  diag ( result_matrix ) = rep ( 0 , n )
  
  return ( result_matrix )
  }
