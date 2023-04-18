#' 
#' Ker.norm
#' computes the values of the normal kernel. Caps values from below at 1e-40, so that no divisions by 0 occur
#'
#' @param x \code{numeric} is the distance between 
#' @export

Ker.norm = function ( x ) { 
  apply ( rbind ( dnorm ( x ) , 
                  rep ( 1e-40 , length ( x ) ) ) , 
          2 , max ) }