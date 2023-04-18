#' predict.mclassiKernel
#' 
#' Predictions with an mclassiKernel object
#'
#' The present function yields predictions for new data with an mclassiKernel object .
#'
#' @param object mclassiKernel object defined with \code{\link[classiMultiFuncTest:mclassiKernel]{mclassiKernel}}
#' @param newdata  the functional covariates for prediction as a \code{list} of \eqn{m} objects in \code{matrix} form. 
#' Each matrix stores a dimension of the set of functions, such that columns are individuals (\eqn{n}) and rows are discrete-time 
#' points (\eqn{t}). Functions' values should be of the same time points as the training data.
#' @param predict.type \code{string} to specify type of prediction. Either \code{"response"} or \code{"prob"}
#' @examples
#'
#' ## 3-dimensional functions
#' #classes
#' classes = as.factor ( c ( "Cat" , "Dog" , "Dog" , "Cat" ) )
#'
#' #training_data
#' x = replicate ( 4 , rnorm ( 100 , 0 , 3 ) )
#' y = replicate ( 4 , rnorm ( 100 , 3 , 1 ) )
#' z = replicate ( 4 , rpois ( 100 , 2 ) )
#' training_data = data.frame ( cbind ( id = 1:4 , dim = 3 , t ( x ), t ( y ) , t ( z ) ) )  
#'
#' #Creating object for mclassiKernel prediction
#' object = mclassiKernel ( classes = classes , fdata = training_data , h = 30 , kernel = "Ker.norm" , nderiv = 0 , cl = NULL )
#'
#'#test_data
#' x = replicate (2, rnorm ( 100 , 0 , 2.9 ) )
#' y = replicate (2, rnorm ( 100 , 3 , 1.5 ) )
#' z = replicate (2, rpois ( 100 , 3 ) )
#' test_data = data.frame ( cbind ( id = 5:6 , dim = 3 , t ( x ), t ( y ) , t ( z ) ) )  
#'
#' #Predict
#'
#' predict.mclassiKernel ( object = object, newdata= test_data, predict.type = "response" )
#' @export

predict.mclassiKernel = function ( object , 
                                   newdata = NULL , 
                                   Mdist = NULL , 
                                   predict.type = "response" )
  { 
  checkmate :: assertDataFrame ( newdata )
  d = unique ( newdata [ , "dim" ] )
  newdata = t ( newdata [ , - which ( names ( newdata ) %in%  c ( "id" , "dim" ) ) ] )
  newdata = plyr :: alply ( cbind ( 1 : d ) ,
                            1 , 
                            function ( i ) cbind ( data.frame ( dim = rep ( 1 : d , each = nrow ( newdata ) / d ) , newdata ) [ data.frame ( dim = rep ( 1 : d , each = nrow ( newdata ) / d ) , newdata ) [ , "dim" ] == i , - 1 ] ) )
  checkmate :: assertList ( newdata )
  if ( sum ( apply ( sapply ( object$fdata , dim ) , 1 , diff ) ) != 0 ) {
    stop("Error: objects in fdata have different dimensions")}
  if ( sum ( apply ( sapply ( newdata , dim ) , 1 , diff ) ) != 0 ) {
    stop ( "Error: objects in newdata have different dimensions" ) }
  checkmate :: assertChoice ( predict.type , choices = c ( "response" , "prob" ) )
  
  if ( object$nderiv != 0 ) {
    object$fdata = lapply ( object$fdata , 
                            function ( x ) apply ( x , 
                                                   2 , 
                                                   function ( y ) base :: diff ( y , lag = object$nderiv ) ) )
    newdata = lapply ( newdata , 
                       function ( x ) apply ( x , 
                                              2 , 
                                              function ( y ) base :: diff ( y , lag = object$nderiv ) ) ) }

  if ( is.null ( object$mdist ) ) {
    mdist = do.call ( compmDistMat , list ( x = object$fdata ,
                                            y = newdata ,
                                            method = object$metric ,
                                            parallel = object$parallel ,
                                            cl = object$cl ,
                                            diag = object$diag ,
                                            upper = object$upper ) ) 
  } else {
      mdist = Mdist }
  ns = sapply ( plyr :: alply ( as.matrix ( mdist ) , 
                                 1 , 
                                 function ( x ) do.call ( object$kernel , list ( x / object$h ) ) ) , 
                function ( x ) tapply( x , object$classes , 
                                       sum , na.rm = TRUE ) )
  ds = sapply ( plyr :: alply ( as.matrix ( mdist ) , 
                                1 , 
                                function ( x ) do.call ( object$kernel , list ( x / object$h ) ) ) , 
                sum , na.rm = TRUE )
  if ( predict.type == "response" ) {
    result = apply ( apply ( cbind ( 1 : length ( ds ) ) , 
                             1 , 
                             function ( i ) ns [ , i ] / ds [ i ] ) ,
                     2 , function ( x ) which.max ( x ) - 1 )
    result = factor ( result , levels = ( ( 1 : length ( levels ( object$classes ) ) ) - 1 ) )
  } else {
      result = t ( apply ( cbind ( 1 : length ( ds ) ) , 
                           1 , 
                           function ( i ) ns [ , i ] / ds [ i ] ) ) }
  return ( result )
  }

